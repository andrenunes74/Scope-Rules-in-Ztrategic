module Examples.AG.Let.Let_Zippers (
                module Examples.AG.Let.SharedAG, 
                dclo, 
                errs, 
                dcli, 
                env, 
                lev, 
                mBIn, 
                mNBIn, 
                expand, 
                semantics) 
        where 
import Language.Grammars.ZipperAG
import Data.Generics.Zipper
import Data.Data
import Examples.AG.Let.SharedAG
import Data.List (sortBy)

-- --------
-- --
-- - Synthesized Attributes
-- --
-- --------

dclo :: Zipper Root ->  Env
dclo ag = case (constructor ag) of
           CRoot       -> dclo (ag.$1)
           CLet        -> dclo (ag.$1)
           CNestedLet  -> dclo (ag.$3)
           CAssign     -> dclo (ag.$3)
           CEmptyList  -> dcli ag

errs :: Zipper Root -> Errors
errs ag = case (constructor ag) of
           CRoot      -> errs (ag.$1)
           CLet       -> (errs (ag.$1)) ++ (errs (ag.$2))
           CAdd       -> (errs (ag.$1)) ++ (errs (ag.$2))
           CSub       -> (errs (ag.$1)) ++ (errs (ag.$2))
           CEmptyList -> []
           CConst     -> []
           CVar       -> mBIn (lexeme_Name ag) (env ag)
           CAssign    -> mNBIn (lexeme_Name ag,lev ag) (dcli ag) 
                            ++ (errs (ag.$2)) ++ (errs (ag.$3))
           CNestedLet -> mNBIn (lexeme_Name ag,lev ag) (dcli ag) 
                            ++ (errs (ag.$2)) ++ (errs (ag.$3))

-- --------
-- --
-- - Inheritted Attributes
-- --
-- --------

dcli :: Zipper Root -> Env
dcli ag = case (constructor ag) of
           CLet  -> case (constructor (parent ag)) of
                         CRoot      -> []
                         CNestedLet -> env  (parent ag)
           _     -> case (constructor (parent ag)) of
                         CLet       -> dcli (parent ag)
                         CAssign    -> (lexeme_Name (parent ag), lev (parent ag), lexeme_Exp $ parent ag) : (dcli (parent ag))
                         CNestedLet -> (lexeme_Name (parent ag), lev (parent ag), Nothing) : (dcli (parent ag))

{-
env :: Zipper Root -> [(String, Zipper Root)]
env ag = case (constructor ag) of
          CRoot -> dclo ag
          CLet  -> case (constructor (parent ag)) of
                        CNestedLet -> dclo ag
                        _          -> env (parent ag)
          _     -> env (parent ag)
-}

env :: Zipper Root -> Env
env ag = case (constructor ag) of
          CRoot -> dclo ag
          CLet  -> dclo ag
          _     -> env (parent ag)


{-
lev :: Zipper Root -> Int
lev ag = case (constructor ag) of
          CRoot -> 0
          CLet  -> 1 + lev (parent ag)
          _     -> lev (parent ag)
-}


lev :: Zipper Root -> Int
lev ag = case (constructor ag) of
           CLet  -> case (constructor $ parent ag) of
                       CNestedLet -> (lev $ parent ag) + 1
                       CRoot      -> 0
           _     -> lev $ parent ag


-- --------
-- --
-- - Environment lookup functions
-- --
-- --------

mBIn :: Name -> Env -> Errors
mBIn name [] = [name]
mBIn name ((n,i,l):es) = if (n==name) then [] else mBIn name es

mNBIn :: (String, Int) -> Env -> Errors
mNBIn t []                   = [] 
mNBIn (n1,l1) ((n2,l2,_):es) = if (n1==n2) && (l1 == l2)
                               then [n1] else mNBIn (n1,l1) es


{-
expand :: (Name,Int) -> Env -> Maybe Exp
expand (n, -1) _ = Nothing 
expand (n, l) e = case expand' (n,l) e of 
      Nothing -> expand (n, l-1) e
      r -> r 
 where expand' (n, l) [] = Nothing 
       expand' (n, l) ((n2,l2,e2):es) = if n==n2 && l==l2 then e2 else expand' (n,l) es
-}

expand :: (Name,Int) -> Env -> Maybe Exp
expand (i, l) e = case results of 
      ((nE, lE, eE):_) -> eE 
      _ -> Nothing 
   where results = sortBy (\(nE1, lE1, _) (nE2, lE2, _) -> compare lE2 lE1) $ 
                    filter (\(nE, lE, _) -> nE == i && lE <= l) e


semantics :: Root -> [String]
semantics p = errs (mkAG p)

