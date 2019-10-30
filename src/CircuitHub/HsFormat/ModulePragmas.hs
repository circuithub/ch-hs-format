{-|

Format the pragmas of a module such that:

1. All pragmas are lowercase (LANGUAGE ➝ language)
2. Pragmas are grouped by pragma type, and separated by one line of whitespace.
3. Each pragma group is sorted alphabetically.
4. The final module comment proceds the module declaration.

-}

module CircuitHub.HsFormat.ModulePragmas where

import CircuitHub.HsFormat
import Data.Char
import Data.List
import Data.Either
import HsExtension ( GhcPs )
import SrcLoc ( Located )
import qualified HsSyn
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Types ( Comment(..), DeltaPos(..), KeywordId(..) )


formatLanguagePragmas :: Formatter ( Located ( HsSyn.HsModule GhcPs ) )
formatLanguagePragmas =
  mapAnnotation $ \moduleAnn -> do
    let
      -- Find all {-# LANGUAGE ... #-} pragmas.
      ( languagePragmas, otherComments1 ) =
        splitComments
          ( \c -> "{-# language" `isPrefixOf` map toLower ( commentContents c ) )
          ( annsDP moduleAnn )

      -- Find all {-# OPTION #-} pragmas
      ( optionPragmas, otherComments2 ) =
        splitComments
          ( \c -> "{-# options" `isPrefixOf` map toLower ( commentContents c ) )
          otherComments1

      -- Form the module header, sorting each group and interspersing each group
      -- with a blank line.
      moduleHeader =
        catHeaderSections
          [ listComments ( sortComments ( map cleanPragma languagePragmas ) )
          , listComments ( sortComments ( map cleanPragma optionPragmas ) )
          , otherComments2
          ]

    return moduleAnn { annsDP = moduleHeader }


-- Cleans a comment by transforming {-# LANGUAGE ➝ {-# language, and also
-- collapsing multiple spaces to one.
cleanPragma :: Comment -> Comment
cleanPragma c =
  c
    { commentContents =
        case words ( commentContents c ) of
          _:t:rest ->
            unwords ( "{-#" : map toLower t :  rest )

          other ->
            unwords other
    }


splitComments :: (Comment -> Bool) -> [(KeywordId, b)] -> ([Comment], [(KeywordId, b)])
splitComments f anns =
        partitionEithers
          ( map
              ( \x@( kw, _ ) ->
                  case kw of
                    AnnComment c ->
                      if f c
                        then Left c
                        else Right x

                    _ ->
                      Right x
              )
              anns
          )


listComments :: [Comment] -> [ ( KeywordId, DeltaPos ) ]
listComments xs =
  case xs of
    pragma1:pragmas ->
      ( AnnComment pragma1, DP ( 0, 0 ) )
        : [ ( AnnComment p, DP ( 1, 0) ) | p <- pragmas ]

    [] ->
      []


sortComments :: [Comment] -> [Comment]
sortComments =
  sortOn ( map toLower . commentContents )


catHeaderSections :: [[(a, DeltaPos)]] -> [(a, DeltaPos)]
catHeaderSections =
  go . filter ( not . null )

  where

    go [] =
      []

    go ( g : gs ) =
      g ++ concatMap ( setFirstOffset ( DP ( 2, 0 ) ) ) gs

    setFirstOffset dp xs =
      case xs of
        (a, _):as ->
          (a, dp) : as

        [] ->
          []
