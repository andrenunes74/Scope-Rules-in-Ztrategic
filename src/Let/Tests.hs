{-# LANGUAGE StandaloneDeriving #-}
module Examples.AG.Let.Tests where

import Data.Maybe (fromJust)
import Language.Grammars.ZipperAG
import Data.Generics.Aliases
import Data.Generics.Zipper
import Library.Ztrategic
import Examples.AG.Let.Let_Zippers
import Examples.AG.Let.Let_Opt_Strategic

--used for quick deriving more tests
{-
deriving instance Show Exp
deriving instance Show List 
deriving instance Show Let 
deriving instance Show Root 
-}
deriving instance Eq Exp
deriving instance Eq List
deriving instance Eq Let
deriving instance Eq Root


test = and [
    test1,
    test2,
    test3,
    test4,
    err1,
    err2,
    err3,
    err6,
    err7
    ]


test1 = res == fromZipper (fromJust $ opt'' $ toZipper ex1)
 where res = Root (Let (Assign "a" (Const 1) (Assign "b" (Const 1) EmptyList)) (Const 2))

test2 = res == fromZipper (fromJust $ opt'' $ toZipper ex2)
 where res = Root (Let (Assign "a" (Const (-13)) (Assign "c" (Const 8) (NestedLet "w" (Let (Assign "z" (Const (-10)) EmptyList) (Const (-7))) (Assign "b" (Const 3) EmptyList)))) (Add (Const (-5)) (Neg (Var "w"))))

test3 = res == fromZipper (fromJust $ opt'' $ toZipper $ Root ex3)
 where res = Root (Let (Assign "a" (Const 3) (Assign "c" (Const 2) (Assign "b" (Const 3) EmptyList))) (Const 8))

test4 = res == fromZipper (fromJust $ opt'' $ toZipper $ Root plev)
 where res = Root (Let (Assign "a" (Const 1) (NestedLet "b" (Let (Assign "c" (Const 2) (Assign "a" (Const 2) EmptyList)) (Const 2)) EmptyList)) (Var "b"))

err1 = [] == errors (toZipper $ ex1)
err2 = [] == errors (toZipper $ ex2)
err3 = [] == errors (toZipper $ Root ex3)
err6 = ["d","a"] == errors (toZipper ex6)
err7 = ["d","w","a"] == errors (toZipper ex7)





-- nº de nesting , nº nested lets em cada level, lista de vars
testTree :: Int -> Int -> [Name] -> Root
testTree a b c = Root $ genLet a b c


genLet :: Int -> Int -> [Name] -> Let
genLet x y l = Let (genList x y l) (Add (aux x y) (genExp l))
        where aux 0 _ = Const 0
              aux _ 0 = Const 0
              aux x y = Add (Var ("nest" ++ show x ++ show y)) (aux x (y-1))


genList :: Int -> Int -> [Name] -> List
genList x y d = aux d
         where aux [] = genNest x y d
               aux (h:t) = Assign h (Const 1) (aux t)

genExp :: [Name] -> Exp
genExp [] = Const 0
genExp [h] = Var h
genExp (h:t) = Add (Var h) (genExp t)

genNest :: Int -> Int -> [Name] -> List
genNest 0 _ _ = EmptyList
genNest _ 0 _ = EmptyList
genNest x y d = NestedLet ("nest" ++ show x ++ show y) (Let (genList (x-1) y d) (Add (aux x y) (genExp d))) (aux2 (y-1))
        where aux 1 _ = Const 0
              aux _ 0 = Const 0
              aux x y = Add (Var ("nest" ++ show (x - 1) ++ show y)) (aux x (y-1))
              aux2 0 = EmptyList
              aux2 yy = NestedLet ("nest" ++ show x ++ show yy) (Let (genList (x-1) y d) (Add (aux x y) (genExp d)) ) (aux2 (yy-1))
