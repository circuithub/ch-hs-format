{-# language GADTs #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module CircuitHub.HsFormat ( Formatter, mapAnnotation, tryAndFormat ) where

import Control.Monad ( (>=>) )
import Data.Foldable ( for_ )
import qualified Data.Map.Strict as Map
import SrcLoc (Located)
import Data.Data
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Types ( mkAnnKey )


type Formatter a =
  a -> Transform a


mapAnnotation :: (Data a, Monad m) => (Annotation -> TransformT m Annotation) -> Located a -> TransformT m (Located a)
mapAnnotation f x = do
  anns <-
    getAnnsT

  for_
    ( Map.lookup annKey anns )
    ( f >=> modifyAnnsT . Map.insert annKey )

  return x

  where

    annKey =
      mkAnnKey x


tryAndFormat :: forall a b. ( Data a, Data b ) => Formatter b -> Formatter a
tryAndFormat format a =
  case eqT @a @b of
    Nothing ->
      return a

    Just Refl ->
      format a
