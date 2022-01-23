{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language NamedFieldPuns #-}

{-|

Format all top level declarations to have two blank lines separating them (but
group type signatures).

-}

module CircuitHub.HsFormat.TopLevelDeclarations ( topLevelDeclarations ) where

import Data.List
import Control.Monad
import SrcLoc
import Data.List.NonEmpty ( NonEmpty(..) )
import GHC.Hs
import CircuitHub.HsFormat
import Data.Foldable
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Types ( DeltaPos(..) )


topLevelDeclarations :: Formatter ( HsModule GhcPs )
topLevelDeclarations hsModule = do
  -- Keep any comments attached to the very last import.
  case ( hsmodImports hsModule, hsmodDecls hsModule ) of
    ( _:_, a:_ ) ->
      balanceComments ( last ( hsmodImports hsModule ) ) a

    _ ->
      return ()

  {-

  Keep trailing comments for declarations.

  We want to prevent

    x :: A
      ...
      where
        foo = ... --bar

    b :: B

  Becoming

    x :: A
      ...
      where
        foo = ...

    --bar
    b :: B

  -}

  zipWithM_
    balanceComments
    ( hsmodDecls hsModule )
    ( tail ( hsmodDecls hsModule ) )

  for_ ( groupDeclarations ( hsmodDecls hsModule ) ) $ \( a :| as ) -> do
    setEntryDPT a ( DP ( 3, 0 ) )

    for_ as ( `setEntryDPT` DP ( 1, 0 ) )

  return hsModule


groupDeclarations :: Eq (IdP pass) => [Located (HsDecl pass)] -> [NonEmpty (Located (HsDecl pass))]
groupDeclarations [] =
  []
groupDeclarations ( x : xs ) =
  finish
  ( foldl
    ( \st@( groups, groupStart :| gs ) decl ->
        if not ( null ( namesOf groupStart `intersect` namesOf decl ) ) then
          ( groups
          , groupStart :| ( gs ++ [ decl ] )
          )

        else
          ( finish st
          , pure decl
          )
    )
    ( [], pure x )
    xs )

  where

    finish ( groups, g :| gs ) =
      groups ++ [ g :| gs ]


namesOf :: Located ( HsDecl pass ) -> [ IdP pass ]
namesOf ( L _ ( SigD _ ( TypeSig _ names _ ) ) ) =
  map unLoc names
namesOf ( L _ ( SigD _ ( InlineSig _ ( L _ name ) _ ) ) ) =
  [ name ]
namesOf ( L _ ( SigD _ ( SpecSig _ ( L _ name ) _ _ ) ) ) =
  [ name ]
namesOf ( L _ ( ValD _ FunBind{ fun_id } ) ) =
  [ unLoc fun_id ]
namesOf ( L _ ( ValD _ VarBind{ var_id } ) ) =
  [ var_id ]
namesOf ( L _ ( KindSigD _ ( StandaloneKindSig _ ( L _ name ) _ ) ) ) =
  [ name ]
namesOf ( L _ ( TyClD _ ( DataDecl _ ( L _ name ) _ _ _ ) ) ) =
  [ name ]
namesOf ( L _ ( TyClD _ ( FamDecl _ ( FamilyDecl _ _ ( L _ name ) _ _ _ _ ) ) ) ) =
  [ name ]
namesOf ( L _ ( TyClD _ ( SynDecl _ ( L _ name ) _  _ _ ) ) ) =
  [ name ]
namesOf _ =
  []
