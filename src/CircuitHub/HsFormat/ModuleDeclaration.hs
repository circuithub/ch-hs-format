{-|

Format the @module ... where@ line of a file, such that.

-}

module CircuitHub.HsFormat.ModuleDeclaration ( moduleDecl ) where

import Control.Monad
import CircuitHub.HsFormat
import Data.Foldable ( for_ )
import ApiAnnotation
import HsExtension ( GhcPs )
import SrcLoc ( Located, getLoc, isOneLineSpan, unLoc )
import qualified HsSyn
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Types ( Comment(..), DeltaPos(..), KeywordId(..) )
import Language.Haskell.GHC.ExactPrint.Utils


moduleDecl :: Formatter ( Located ( HsSyn.HsModule GhcPs ) )
moduleDecl hsModule = do
  for_
    ( HsSyn.hsmodExports ( unLoc hsModule ) )
    formatExports

  mapAnnotation
    ( \ann ->
        let
          -- Find the DP to the end of all module comments. This is used to find
          -- the offset for the @module@ keyword.
          commentDps =
            foldMap
              ( \(kwid, dp) ->
                  case kwid of
                    AnnComment c ->
                      [ dp `addDP` dpFromString (commentContents c) ]

                    _ ->
                      []
              )
              ( takeWhile ( ( /= G AnnModule ) . fst ) ( annsDP ann ) )

          -- Compute a new set of annotations for module syntax.
          annsDP' =
            map
              ( \( kwid, dp ) ->
                  case kwid of
                    -- If there are any prior comments, the module keyword is two
                    -- lines under the last comment. Otherwise, it is exactly at
                    -- the location of the element (the start of the file).
                    G AnnModule ->
                      ( kwid
                      , foldl addDP ( DP ( 0, 0 ) ) commentDps
                          `addDP` if null commentDps then DP (0, 0) else DP (2, 0)
                      )

                    -- AnnVal is location of the name of the module. It is located
                    -- one space after the module keyword.
                    G AnnVal ->
                      ( kwid, DP ( 0, 1 ) )

                    -- The where keyword directly follows the end of the list of
                    -- exports or module name.
                    G AnnWhere ->
                      ( kwid, DP ( 0, 1 ) )

                    G AnnCloseP ->
                      ( kwid, DP ( 0, 1 ) )

                    _ ->
                      ( kwid, dp )
              )
              ( annsDP ann )

        in
        return ann { annsDP = annsDP' }
    )
    hsModule


formatExports :: Formatter ( Located [ HsSyn.LIE GhcPs ] )
formatExports lies = do
  zipWithM_ balanceComments ( unLoc lies ) ( tail ( unLoc lies ) )

  -- Multiline exports are formatted under the module name, whereas single
  -- line exports are formatted inline.
  setEntryDPT
    lies
    ( if multiline then DP ( 1, 2 ) else DP ( 0, 1 ) )

  for_ ( unLoc lies ) formatIE

  formatParens

  where

    multiline =
      not ( isOneLineSpan ( getLoc lies ) )

    formatParensNonempty ann =
      let
        annsDP' =
          map
            ( \( kwid, dp ) ->
                case kwid of
                  G AnnOpenP ->
                    ( kwid, DP ( 0, 0 ) )

                  G AnnCloseP ->
                    ( kwid, DP ( if multiline then ( 1, 2 ) else ( 0, 1 ) ) )

                  _ ->
                    ( kwid, dp )
            )
            ( annsDP ann )

      in
      return ann { annsDP = annsDP' }

    formatParensEmpty ann =
      let
        annsDP' =
          map
            ( \( kwid, dp ) ->
                case kwid of
                  G AnnOpenP ->
                    ( kwid, DP ( 0, 0 ) )

                  G AnnCloseP ->
                    ( kwid, DP ( 0, 0 ) )

                  _ ->
                    ( kwid, dp )
            )
            ( annsDP ann )

      in
      return ann { annsDP = annsDP' }

    formatParens =
      mapAnnotation
        ( \ann ->
            case unLoc lies of
              _:_ ->
                formatParensNonempty ann

              [] ->
                formatParensEmpty ann
        )
        lies

    formatIE =
      mapAnnotation
        ( \ann -> do
            let
              -- If this IE has comments before the comma, accumulate the delta.
              -- If we don't do this, the comma will end up on the line of the
              -- comment, creating invalid syntax.
              commentDP =
                dpToComma ann

              -- If this IE has annPriorComments it's the first IE. In this case
              -- the entry of the IE is one line below indented 3 spaces (the
              -- comma that would normally be there is a space).
              entryDP =
                case annPriorComments ann of
                  [] ->
                    DP ( 0, 1 )

                  _ ->
                    foldl
                      addDP
                      ( DP ( 1, 3 ) )
                      ( map snd ( annPriorComments ann ) )

              annsDP' =
                map
                  ( \( kwid, dp ) ->
                      case kwid of
                        G AnnComma ->
                          ( kwid
                          , if multiline then
                              addDP commentDP ( DP ( 1, 2 ) )
                            else
                              DP ( 0, 0 )
                          )

                        _ ->
                          ( kwid, dp )
                  )
                  ( annsDP ann )

            return ann { annEntryDelta = entryDP, annsDP = annsDP' }
        )

    dpToComma ann =
      foldl
        addDP
        ( DP ( 0, 0 ) )
        ( map snd ( takeWhile ( ( G AnnComma /= ) . fst ) ( annsDP ann ) ) )
