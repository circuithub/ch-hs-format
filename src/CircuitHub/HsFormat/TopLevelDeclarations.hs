{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language NamedFieldPuns #-}

{-|

Format all top level declarations to have two blank lines separating them (but
group type signatures).

-}

module CircuitHub.HsFormat.TopLevelDeclarations ( topLevelDeclarations ) where

import Control.Monad
import SrcLoc
import Data.List.NonEmpty ( NonEmpty(..) )
import HsSyn
import CircuitHub.HsFormat
import Data.Foldable
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Types ( DeltaPos(..) )


topLevelDeclarations :: Formatter ( HsModule GhcPs )
topLevelDeclarations hsModule = do
  zipWithM_
    balanceComments
    ( hsmodDecls hsModule )
    ( tail ( hsmodDecls hsModule ) )

  for_ ( groupDeclarations ( hsmodDecls hsModule ) ) $ \declGroup ->
    case declGroup of
      a :| as -> do
        setEntryDPT a ( DP ( 3, 0 ) )

        for_ as ( `setEntryDPT` ( DP ( 1, 0 ) ) )

  return hsModule


groupDeclarations :: Eq (IdP pass) => [GenLocated l (HsDecl pass)] -> [NonEmpty (GenLocated l (HsDecl pass))]
groupDeclarations [] =
  []
groupDeclarations ( x : xs ) =
  finish
  ( foldl
    ( \st@( groups, groupStart :| gs ) decl ->
        case ( groupStart, decl ) of
          ( L _ ( SigD s ), L _ ( ValD b ) ) | b `bindMatches` s ->
            ( groups
            , groupStart :| ( gs ++ [ decl ] )
            )

          ( L _ ( ValD FunBind{ fun_id = L _ n1 } ), L _ ( ValD FunBind{ fun_id = L _ n2 } ) ) | n1 == n2 ->
            ( groups
            , groupStart :| ( gs ++ [ decl ] )
            )

          _ ->
            ( finish st
            , pure decl
            )
    )
    ( [], pure x )
    xs )

  where

    finish ( groups, g :| gs ) =
      groups ++ [ g :| gs ]


bindMatches :: (Eq (IdP pass), IdP idL ~ IdP pass) => HsBindLR idL idR -> Sig pass -> Bool
bindMatches ( FunBind{ fun_id } ) ( TypeSig names _ ) =
  unLoc fun_id `elem` map unLoc names
bindMatches ( VarBind{ var_id } ) ( TypeSig names _ ) =
  var_id `elem` map unLoc names
bindMatches _ _ =
  False
