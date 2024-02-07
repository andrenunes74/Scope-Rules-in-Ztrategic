{-Module that contains optimizations to the functions of the parser-}
{-# LANGUAGE DeriveDataTypeable #-}
module Opt where    
import Core
import Data.Data
import Data.Generics.Zipper
import Library.Ztrategic
import Library.StrategicData (StrategicData)

instance StrategicData [(Item,String)]

treeTest = [(OpenFuncao (DefFuncao (Name "mht") [Arg (Const 15),Arg (Var "wbk"),Arg (Const 6)] [Arg (Add (Const 0) (Var "qhpzs"))]),"")]

--Optimize exps
optExp :: Exp -> Exp
--Null element of addition and subtraction
optExp (Add e (Const 0)) = e
optExp (Sub e (Const 0)) = e
optExp (Add (Const 0) e) = e
optExp (Sub (Const 0) e) = e
--Algebric proprieties
optExp (Add (Const a) (Const b)) = (Const (a+b))
optExp (Mul (Const a) (Const b)) = (Const (a*b))
optExp (Sub (Const a) (Const b)) = (Const (a-b))
optExp (Div (Const a) (Const b)) = (Const (a `div` b))
optExp (Inc (Const a)) = (Const (a+1))
optExp (Dec (Const a)) = (Const (a-1))
--Null and absorbing element of multiplication and division
optExp (Mul e (Const 0)) = (Const 0)
optExp (Mul (Const 0) e) = (Const 0)
optExp (Mul e (Const 1)) = e
optExp (Mul (Const 1) e) = e
optExp (Div (Const 0) e) = (Const 0)
optExp (Div e (Const 1)) = e
--Logic optimizations
optExp (Equals a b) = if a==b then (Bool True) else (Bool False)
optExp (Less (Const a) (Const b)) = if a<b then (Bool True) else (Bool False)
optExp (Greater (Const a) (Const b)) = if a>b then (Bool True) else (Bool False)
optExp (GTE (Const a) (Const b)) = if a>=b then (Bool True) else (Bool False)
optExp (LTE (Const a) (Const b)) = if a<=b then (Bool True) else (Bool False)
optExp (Not (Const a)) = (Const (-a))
optExp (Not (Bool True)) = (Bool False)
optExp (Not (Bool False)) = (Bool True)
optExp (And (Bool True) (Bool True)) = (Bool True)
optExp (And (Bool False) (Bool False)) = (Bool False)
optExp (And (Bool True) (Bool False)) = (Bool False)
optExp (And (Bool False) (Bool True)) = (Bool False)
optExp (Or (Bool False) (Bool False)) = (Bool False)
optExp (Or (Bool True) (Bool True)) = (Bool True)
optExp (Or (Bool True) (Bool False)) = (Bool True)
optExp (Or (Bool False) (Bool True)) = (Bool True)
optExp e = e

--Optimize items
optItem :: Item -> Item
optItem (Decl a b) = (Decl a (optExp b))
optItem (Arg a) = (Arg (optExp a))
optItem (Increment a) = (Arg (optExp a))
optItem (Decrement a) = (Arg (optExp a))
optItem (NestedIf (If a b)) = (NestedIf (If (optExp a) (map optItem b)))
optItem (OpenIf (If a b)) = (OpenIf (If (optExp a) (map optItem b)))
optItem (NestedWhile (While a b)) = (NestedWhile (While (optExp a) (map optItem b)))
optItem (OpenWhile (While a b)) = (OpenWhile (While (optExp a) (map optItem b)))
optItem (NestedLet (Let a b)) = (NestedLet (Let (map optItem a) (optExp b)))
optItem (OpenLet (Let a b)) = (OpenLet (Let (map optItem a) (optExp b)))
optItem (OpenFuncao (DefFuncao a b c)) = (OpenFuncao (DefFuncao a b (map optItem (map optItem c))))
optItem (NestedReturn a) = (NestedReturn (optExp a))
optItem a = a

--Optimize with zippers (Strategic optimization)
optItemz :: Item -> Maybe Item
optItemz a = Just (optItem a)

--full top down strategie
optWithZippersTD lista = 
    let listaZipper = toZipper lista
        Just listaNova = applyTP (full_tdTP step) listaZipper
            where step = idTP `adhocTP` optItemz
        in 
        fromZipper listaNova

--full bottom up strategie
optWithZippersBU lista = 
    let listaZipper = toZipper lista
        Just listaNova = applyTP (full_buTP step) listaZipper
            where step = idTP `adhocTP` optItemz
        in 
        fromZipper listaNova
