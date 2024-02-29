{-# LANGUAGE DeriveDataTypeable #-}

module Let_Interface where
import qualified Block.Shared as B
import qualified ToBlock as TB 
import qualified Interface as I 
import qualified Let.Shared as L 
import qualified Let.SharedAG as LS
import Data.Data
import Data.Generics.Zipper
import Library.StrategicData (StrategicData)
import Library.Ztrategic
import Data.Generics.Aliases
import Library.ZipperAG
import Data.Maybe
import Debug.Trace

p :: L.Let
p = L.Let ( L.Assign "a" (L.Add (L.Var "b") (L.Const (-16)))
           $ L.Assign "c" (L.Const 8)
           $ L.NestedLet "w" ( L.Let (L.Assign "z" (L.Add (L.Var "a") (L.Var "b")) L.EmptyList)
                            $ (L.Add (L.Var "z") (L.Var "b")))
           $ L.Assign "b" (L.Sub ((L.Add (L.Var "c") (L.Const 3))) (L.Var "c"))
             L.EmptyList)
      $ (L.Sub (L.Add (L.Var "c") (L.Var "a")) (L.Var "w"))

instance I.Scopes (L.Let) where
    isDecl ag = case (LS.constructor ag) of
        LS.CAssign -> True
        _ -> False
    isUse ag = case (LS.constructor ag) of
        LS.CVar -> True
        _ -> False
    isBlock ag = case (LS.constructor ag) of
        LS.CLet -> True
        LS.CNestedLet -> True
        _ -> False

build :: I.Scopes a => Zipper a -> B.P
build a = B.Root (I.buildChildren build' a)

build' :: I.Scopes a => Zipper a -> B.Its
build' a  | I.isDecl a = B.ConsIts (B.Decl (LS.lexeme_Name a)) (TB.mergeIts (build' $ a.$2) (build' $ a.$3))
          | I.isUse a = B.ConsIts (B.Use (LS.lexeme_Name a)) B.NilIts
          | I.isBlock a = case (LS.constructor a) of
                        LS.CLet -> B.ConsIts (B.Block $ I.buildChildren build' a) B.NilIts
                        LS.CNestedLet -> B.ConsIts (B.Decl (LS.lexeme_Name a)) (TB.mergeIts (build' $ a.$2) (build' $ a.$3))
          | otherwise = case (LS.constructor a) of
                            LS.CConst -> B.NilIts
                            _ -> I.buildChildren build' a

main''' :: L.Let -> B.P
main''' a = build $ mkAG a