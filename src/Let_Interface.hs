{-# LANGUAGE DeriveDataTypeable #-}

module Let_Interface where
import qualified Block.Shared as B
import qualified IScopes as I 
import qualified Let.Shared as L 
import qualified Let.SharedAG as LS
import qualified Block.Block_Zippers as M
import Data.Data
import Data.Generics.Zipper
import Library.StrategicData (StrategicData(..), isJust)
import Library.Ztrategic
import Data.Generics.Aliases
import Library.ZipperAG
import Data.Maybe hiding (isJust)
import Debug.Trace

instance I.Scopes (L.Let) where
    isDecl ag = case (LS.constructor ag) of
        LS.CAssign -> True
        LS.CNestedLet -> True
        _ -> False
    isUse ag = case (LS.constructor ag) of
        LS.CVar -> True
        _ -> False
    isBlock ag = case (LS.constructor ag) of
        LS.CLet -> True
        _ -> False

instance StrategicData (L.Let) where
    isTerminal t = isJust (getHole t :: Maybe Int)
                || isJust (getHole t :: Maybe LS.Name)

build :: I.Scopes a => Zipper a -> B.P
build a = B.Root (I.buildChildren build' a [])

build' :: I.Scopes a => Zipper a -> B.Directions -> B.Its
build' a d | I.isDecl a = B.ConsIts (B.Decl (LS.lexeme_Name a) d) (I.buildChildren build' a d)
           | I.isUse a = B.ConsIts (B.Use (LS.lexeme_Name a) d) B.NilIts
           | I.isBlock a = B.ConsIts (B.Block $ I.buildChildren build' a d) B.NilIts
           | otherwise = (I.buildChildren build' a d)

main''' a = build $ mkAG a

main''''' a = M.block $ build $ mkAG a

dir a b = LS.lexeme_Name $ I.applyDirections (mkAG a) b

