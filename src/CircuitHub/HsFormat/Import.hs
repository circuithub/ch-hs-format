{-# language DisambiguateRecordFields #-}
{-# language NamedFieldPuns #-}

module CircuitHub.HsFormat.Import where

import ApiAnnotation
import CircuitHub.HsFormat
import Data.Char
import Data.Foldable ( for_ )
import Data.List ( sortOn )
import Data.Maybe
import Data.Traversable ( for )
import FastString ( headFS )
import HsExtension ( GhcPs )
import qualified HsSyn
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Types ( DeltaPos(..), KeywordId(..) )
import OccName ( OccName, isDataOcc, isSymOcc, occNameFS, occNameString )
import RdrName ( RdrName, rdrNameOcc )
import SrcLoc ( GenLocated(..), Located, isOneLineSpan )


formatImport :: Formatter ( Located ( HsSyn.ImportDecl GhcPs ) )
formatImport ( L loc i@HsSyn.ImportDecl{ ideclName, ideclHiding }) = do
  setEntryDPT ideclName ( DP ( 0, 1 ) )

  ideclHiding' <-
    for ideclHiding $ \( hiding, names ) -> do
      let
        multiline =
          case names of
            L loc _ ->
              not ( isOneLineSpan loc )

        hasNames =
          case names of
            L _ [] ->
              False

            L _ _ ->
              True

      if multiline
        then  setEntryDPT names ( DP ( 1, 2 ) )
        else setEntryDPT names ( DP ( 0, 1 ) )

      flip mapAnnotation names $ \ann ->
        let
          annsDP' =
            map
              ( \( kwid, dp ) ->
                  case kwid of
                    G AnnHiding ->
                      ( kwid, DP ( 0, 0 ) )

                    G AnnOpenP | hiding ->
                      ( kwid, DP ( 0, 1 ) )

                    G AnnOpenP | otherwise ->
                      ( kwid, DP ( 0, 0 ) )

                    G AnnCloseP | multiline ->
                      ( kwid, DP ( 1, 2 ) )

                    G AnnCloseP | hasNames ->
                      ( kwid, DP ( 0, 1 ) )

                    G AnnCloseP | otherwise ->
                      ( kwid, DP ( 0, 0 ) )

                    _ ->
                      ( kwid, dp )
              )
              ( annsDP ann )

        in
        return ann { annsDP = annsDP' }

      let
        sortedNames =
          sortOn importName <$> names

      case reverse <$> sortedNames of
        L _ ( x : xs ) -> do
          flip mapAnnotation x $ \ann ->
            let
              annsDP' =
                mapMaybe
                  ( \( kwid, dp ) ->
                      case kwid of
                        G AnnComma ->
                          Nothing

                        _ ->
                          Just ( kwid, dp )
                  )
                  ( annsDP ann )

            in
            return ann { annsDP = annsDP' }

          for_ xs $ mapAnnotation $ \ann ->
            let
              annsDP' =
                ( G AnnComma, DP ( 0, 0 ) )
                : mapMaybe
                    ( \( kwid, dp ) ->
                        case kwid of
                          G AnnComma ->
                            Nothing

                          _ ->
                            Just ( kwid, dp )
                    )
                    ( annsDP ann )

            in
            return ann { annsDP = annsDP' }

        _ ->
          return ()


      case sortedNames of
        L _ names ->
          if multiline
            then
              for_ names $ \name -> do
                setEntryDPT name ( DP ( 0, 1 ) )

                flip mapAnnotation name $ \ann ->
                  let
                    annsDP' =
                      map
                        ( \( kwid, dp ) ->
                            case kwid of
                              G AnnComma ->
                                ( kwid, DP ( 1, 2 ) )

                              _ ->
                                ( kwid, dp )
                        )
                        ( annsDP ann )

                  in
                  return ann { annsDP = annsDP' }

            else
              for_ names ( `setEntryDPT` ( DP ( 0, 1 ) ) )

      return ( hiding, sortedNames )

  flip mapAnnotation ( L loc i { HsSyn.ideclHiding = ideclHiding' } ) $ \ann ->
    let
      annsDP' =
        map
          ( \( kwid, dp ) ->
              case kwid of
                G AnnImport ->
                  ( kwid, DP ( 0, 0 ) )

                G AnnQualified ->
                  ( kwid, DP ( 0, 1 ) )

                G AnnAs ->
                  ( kwid, DP ( 0, 1 ) )

                _ ->
                  ( kwid, dp )
          )
          ( annsDP ann )

    in
    return ann { annsDP = annsDP' }


importName :: Located ( HsSyn.IE GhcPs ) -> ( Bool, Bool, RdrName )
importName ( L _ ( HsSyn.IEVar _ name ) ) = wrappedName name
importName ( L _ ( HsSyn.IEThingAbs _ name ) ) = wrappedName name
importName ( L _ ( HsSyn.IEThingAll _ name ) ) = wrappedName name
importName ( L _ ( HsSyn.IEThingWith _ name _ _ _ ) ) = wrappedName name


wrappedName :: Located ( HsSyn.IEWrappedName RdrName ) -> ( Bool, Bool, RdrName )
wrappedName ( L _ ( HsSyn.IEName ( L _ name ) ) ) =
  ( not ( isUpper ( headFS ( occNameFS (rdrNameOcc name) ) )
                      || headFS ( occNameFS (rdrNameOcc name) ) == ':' )
  , not ( isSymOcc ( rdrNameOcc name) )
  , name
  )
