{-# language DisambiguateRecordFields #-}
{-# language NamedFieldPuns #-}

module CircuitHub.HsFormat.IE where

import CircuitHub.HsFormat

import RdrName (rdrNameOcc)
import OccName (occNameString)
import Data.Traversable ( for )
import Data.Foldable ( for_ )
import ApiAnnotation
import HsExtension ( GhcPs )
import SrcLoc (Located, GenLocated(..), isOneLineSpan)
import qualified HsSyn
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Types ( DeltaPos(..), KeywordId(..) )
import Data.List (sortOn)

formatIE :: Formatter ( Located ( HsSyn.IE GhcPs ) )
formatIE ie = do
  everything <-
    case ie of
      L _ ( HsSyn.IEThingWith _ _ HsSyn.NoIEWildcard ns _ ) ->
        False <$ for_ ns ( `setEntryDPT` ( DP ( 0, 1 ) ) )

      _ ->
        return True

  flip mapAnnotation ie $ \ann ->
    let
      annsDP' =
        map
          ( \( kwid, dp ) ->
              case kwid of
                G AnnOpenP ->
                  ( kwid, DP ( 0, 0 ) )

                G AnnDotdot ->
                  ( kwid, DP ( 0, 0 ) )

                G AnnCloseP | everything ->
                  ( kwid, DP ( 0, 0 ) )

                G AnnCloseP | otherwise ->
                  ( kwid, DP ( 0, 1 ) )

                _ ->
                  ( kwid, dp )
          )
          ( annsDP ann )

    in
    return ann { annsDP = annsDP' }
