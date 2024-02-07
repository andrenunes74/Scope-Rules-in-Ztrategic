{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables #-}
module Library.Memo.AGMemo where 

import Data.Generics.Zipper
import Data.Generics.Aliases
import Library.ZipperAG
import Data.Data
import Data.Maybe (fromJust)

import Debug.Trace

mkAG :: Data x => x -> Zipper x
mkAG = toZipper

-- MemoAG

eval .@. t      = let (v,t') = eval t
                  in  (v, parent t') 

atParent eval t = let n = arity t 
                      (v,t') = eval (parent t)
                  in  (v, t'.$n)

atRight eval t  = let (v,t') = eval (t.$>1)
                  in  (v, (t'.$<1))

atLeft eval t   = let (v,t') = eval (t.$<1)
                  in  (v, (t'.$>1))

class Memo att m a where
  mlookup :: att -> m -> Maybe a
  massign :: att -> a -> m -> m

class (Typeable dtype, Typeable m) => Memoizable dtype m where 
  getMemoTable :: dtype m -> m 
  updMemoTable :: (m -> m) -> dtype m -> dtype m
  cleanMemoTable :: Zipper (dtype m) -> Zipper (dtype m)
  cleanMemoTable = id 

type AGTree_m dtype m a = Zipper (dtype m) -> (a, Zipper (dtype m)) 


memo :: (Memoizable dtype m, Memo attr m a) => 
        attr -> 
        AGTree_m dtype m a -> 
        AGTree_m dtype m a
memo attr eval z =
   case mlookup attr (memoTable z) of
    Just v   -> (v,z)
    Nothing  -> let (v,z') = eval z
                in  (v, transTree attr v z') 
                -- in  (v, trans (ttree attr v) z') 
{-
ttree :: forall dtype m attr a b. (Memoizable dtype m, Memo attr m a) => attr -> a -> (b -> b)
ttree attr v = let x = (massign attr v) :: m -> m
                   f = (updMemoTable x) :: (dtype m -> dtype m)
               in mkT f
-}


memoTable :: forall dtype m. (Memoizable dtype m) => Zipper (dtype m) -> m
memoTable zx = let a' = (fromJust $ getHole zx) :: dtype m
               in getMemoTable a' 


upd :: (Memoizable dtype m, Memo att m a) => att -> a -> Zipper (dtype m) -> dtype m
upd attr v = updMemoTable (massign attr v) . fromJust . getHole 

transTree :: (Memoizable dtype m, Memo att m a) => att -> a -> Zipper (dtype m)
                                     -> Zipper (dtype m)
transTree attr v z = setHole (upd attr v z) z