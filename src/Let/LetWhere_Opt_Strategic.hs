{-# LANGUAGE DeriveDataTypeable #-}
module Examples.AG.Let.LetWhere_Opt_Strategic where

{-
import Examples.AG.Let.Let_Zippers
-}
import Library.Ztrategic
import Data.Maybe (fromJust)
import Language.Grammars.ZipperAG
import Data.Generics.Aliases
import Data.Generics.Zipper
import Data.Data

import Library.StrategicData

-- --------
-- --
-- - Datatypes
-- --
-- --------

data Prog = Root LetWhere
          deriving (Show, Data, Typeable)

data LetWhere = LW Let Where
          deriving (Show, Data, Typeable)

type Where = [(String,Int)]


data Let = Let List Exp
          deriving (Show, Data, Typeable)

data  List  
      = NestedLet  String Let List
      | Assign     String Exp List
      | EmptyList
          deriving (Show, Data, Typeable)

data Exp = Add Exp Exp
         | Sub Exp Exp
         | Neg Exp
         | Var String
         | Const Int
          deriving (Show, Data, Typeable)

instance StrategicData Prog where 
  isTerminal t = False  


{-
p = let  a = 3 *b + 0;
    in  a + a;
 where b = 1;
-}

pp = Root $ 
        LW (
            Let (Assign "a" (Add (Add (Const 3) (Var "b")) (Const 0)) EmptyList)
            (Add (Var "a") (Var "a")))
        [("b", 1)] -- where


ppWError = Root $ 
        LW (
            Let (Assign "a" (Add (Add (Const 3) (Var "b")) (Const 0)) EmptyList)
            (Add (Var "a") (Var "a")))
        [("c", 1)] -- where

zpp = toZipper pp 
zppE = toZipper ppWError


test1 = errs zpp -- we detect no errors on the definition of pp, which is correct
test2 = errs zppE -- we detect errors due to b not being defined - but why is "b" an error 3 times? Explanation below: 

zppEOptimized = fromZipper $ fromJust $ opt'' zppE
zppEO = Root (LW (Let (Assign "a" (Add (Const 3) (Var "b")) EmptyList) (Add (Add (Const 3) (Var "b")) (Add (Const 3) (Var "b")))) [("c",1)])
--Due to opt'', the 2 usages of "a" are replaced by "3+b". Therefore, the optimized tree has 3 invalid usages of "b". 


-- --------
-- --
-- - Strategies
-- --
-- --------

opt'' :: Zipper Prog -> Maybe (Zipper Prog)
opt'' r = applyTP (innermost step) r
 where step = failTP `adhocTPZ` expC `adhocTP` expr  

expC :: Exp -> Zipper Prog -> Maybe Exp
expC (Var i) z = case lookup i (env z) of 
    Just x -> lexeme_Assign2 x
    Nothing -> Nothing 
expC _ z = Nothing 

expr :: Exp -> Maybe Exp
expr (Add e (Const 0))          = Just e 
expr (Add (Const 0) t)          = Just t  
expr (Add (Const a) (Const b))  = Just (Const (a+b)) 
expr (Sub a b)                  = Just (Add a (Neg b))    
expr (Neg (Neg f))              = Just f           
expr (Neg (Const n))            = Just (Const (-n)) 
expr _                          = Nothing

-- --------
-- --
-- - AG
-- --
-- --------

data Constructor = CRoot
                 | CLet
                 | CNestedLet
                 | CAssign
                 | CEmptyList
                 | CAdd
                 | CSub
                 | CNeg
                 | CConst
                 | CVar
                 | CLW
                deriving Show

errs :: Zipper Prog -> [String]
errs ag = case (constructor ag) of
    CLW        -> errs $ fromJust $ opt'' (ag.$1) -- we apply the optimization here
    CRoot      -> errs (ag.$1)
    CLet       -> (errs (ag.$1)) ++ (errs (ag.$2))
    CAdd       -> (errs (ag.$1)) ++ (errs (ag.$2))
    CSub       -> (errs (ag.$1)) ++ (errs (ag.$2))
    CEmptyList -> []
    CConst     -> []
    CVar       -> mBIn (lexeme ag) (env ag)
    CAssign    -> mNBIn (lexeme ag, ag) (dcli ag) 
                     ++ (errs (ag.$2)) ++ (errs (ag.$3))
    CNestedLet -> mNBIn (lexeme ag, ag) (dcli ag) 
                     ++ (errs (ag.$2)) ++ (errs (ag.$3))


dcli :: Zipper Prog -> [(String, Zipper Prog)]
dcli ag = case (constructor ag) of
           CRoot -> []
           CLW   -> env (parent ag)
           CLet  -> case (constructor (parent ag)) of
                         CLW        -> env (parent ag) -- this is new!! should grab the "where" clause values
                         CNestedLet -> env  (parent ag)
           _     -> case (constructor (parent ag)) of
                         CAssign    -> (dcli (parent ag)) 
                            ++ [(lexeme (parent ag), parent ag)]
                         CNestedLet -> (dcli (parent ag)) 
                            ++ [(lexeme (parent ag), parent ag)]
                         _          -> dcli (parent ag)

dclo :: Zipper Prog -> [(String, Zipper Prog)]
dclo ag = case (constructor ag) of
           CRoot       -> dclo (ag.$1)
           CLW         -> dclo (ag.$1)
           CLet        -> dclo (ag.$1)
           CNestedLet  -> dclo (ag.$3)
           CAssign     -> dclo (ag.$3)
           CEmptyList  -> dcli ag

env :: Zipper Prog -> [(String, Zipper Prog)]
env ag = case (constructor ag) of
          CLW   -> let a = (fromJust $ ((getHole (ag.$2)) :: Maybe [(String, Int)])) 
                       in zip (map fst a) (repeat (ag.$2)) --environment of a LW is a list of [(String, Zipper)], where string is varname... -- fix this!!
          CLet  -> dclo ag
          _     -> env (parent ag)



lev :: Zipper Prog -> Int
lev ag = case (constructor ag) of
          CLW   -> 0
          CLet  -> 1 + lev (parent ag)
          _     -> lev (parent ag)


lexeme :: Zipper a -> String
lexeme ag = case (getHole ag :: Maybe List) of
                          Just (Assign    v _ _) -> v
                          Just (NestedLet v _ _) -> v
                          _ -> case (getHole ag :: Maybe Exp) of
                                      Just (Var s) -> s 
                                      _ -> error "Error in lexeme!"

constructor :: Zipper a -> Constructor
constructor ag = case (getHole ag :: Maybe Prog) of
                   Just (Root _) -> CRoot
                   _ -> case (getHole ag :: Maybe Let) of
                          Just (Let _ _) -> CLet
                          _ -> case (getHole ag :: Maybe List) of
                                 Just (NestedLet _ _ _   ) -> CNestedLet
                                 Just (Assign _ _ _) -> CAssign
                                 Just (EmptyList       ) -> CEmptyList
                                 _ -> case (getHole ag :: Maybe Exp) of
                                        Just (Add _ _) -> CAdd
                                        Just (Sub _ _) -> CSub
                                        Just (Neg _  ) -> CNeg
                                        Just (Var _  ) -> CVar
                                        Just (Const _) -> CConst
                                        _              -> case (getHole ag :: Maybe LetWhere) of
                                                            Just (LW _ _) -> CLW 
                                                            _             -> error "Error in constructor!!"

lexeme_Assign2 :: Zipper a -> Maybe Exp
lexeme_Assign2 ag = case (getHole ag :: Maybe List) of
                          Just(Assign _ e _) -> Just e
                          _ -> case (getHole ag :: Maybe Where) of
                            Just l -> Nothing -- we disable optimizations when the values come from the external "where"... for now
                            _ -> Nothing 


mBIn :: String -> [(String, Zipper Prog)] -> [String]
mBIn name [] = [name]
mBIn name ((n,l):es) = if (n==name) then [] else mBIn name es

mNBIn :: (String, Zipper Prog) -> [(String, Zipper Prog)] -> [String]
mNBIn tuple [] = [] 
mNBIn (a1,r1) ((a2,r2):es) = if (a1==a2) && (lev r1 == lev r2) then [a1] else mNBIn (a1,r1) es