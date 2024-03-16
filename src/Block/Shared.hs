{-# LANGUAGE DeriveDataTypeable #-}

-----------------------------------------------------------------------
--
--  The Fun of Programming With Attribute Grammars
--
--  shared code of the Block solutions
--
----------------------------------------------------------------------

module Block.Shared where

import Library.ZipperAG
import Data.Generics.Zipper
import Data.Maybe
import Data.Data
import Data.List

data Direction = U | D | R | L deriving(Show,Eq,Data)
type Directions = [Direction]

data P  = Root Its
          deriving (Typeable, Data,Eq)

data Its = ConsIts It Its
         | NilIts
       deriving (Typeable, Data,Eq)

data It = Decl Name Directions
        | Use Name Directions
        | Block Its
        deriving (Typeable, Data,Eq)

type Name = String


instance Show P where
   show = showRoot

showRoot (Root its) = "[ " ++ showIts its ++ " ]\n"

instance Show Its where
   show = showIts


showIts (ConsIts it NilIts) = show it
showIts (ConsIts it its)    = show it ++ "," ++ showIts its
showIts NilIts              = ""

instance Show It where
  show = showIt

showIt (Decl s d) = "(Decl " ++ s ++ " | Path: " ++ show d ++ ")"
showIt (Use s d) = "(Use " ++ s ++ " | Path: " ++ show d ++ ")"
showIt (Block its) = "[ " ++ showIts its ++ " ]"

type Env    = [(Name, Int)]
type Errors = [Name]


mustBeIn :: Name -> Env -> Errors
mustBeIn n e = if null (filter ((== n) . fst) e) then [n] else []


mustBeIn' n e  | null (filter ((== n) . fst) e)  =  [n] 
               | otherwise                       =  []

mustNotBeIn :: (Name,Int) -> Env -> Errors
mustNotBeIn p e = if p `elem` e then [fst p] else []

mustNotBeIn' p l = nub [ fst p  |  e <- l , e == p ]

mustNotBeIn'' :: (Name,Int) -> Env -> Errors
mustNotBeIn'' p [] = []
mustNotBeIn'' p (e:es) = if p == e then [fst p] else mustNotBeIn p es


