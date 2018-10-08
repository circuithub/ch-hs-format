{-|

Format the @module ... where@ line of a file, such that.

-}

module CircuitHub.HsFormat.ModuleDeclaration ( moduleDecl ) where

import CircuitHub.HsFormat

import ApiAnnotation
import HsExtension ( GhcPs )
import SrcLoc (Located)
import qualified HsSyn
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Types ( DeltaPos(..), KeywordId(..), Comment(..) )
import Language.Haskell.GHC.ExactPrint.Utils


moduleDecl :: Formatter ( Located ( HsSyn.HsModule GhcPs ) )
moduleDecl =
  mapAnnotation $ \ann ->
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

                _ ->
                  ( kwid, dp )
          )
          ( annsDP ann )

    in
    return ann { annsDP = annsDP' }
