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

data P  = Root Its
          deriving (Typeable, Data,Eq)

data Its = ConsIts It Its
         | NilIts
       deriving (Typeable, Data,Eq)

data It = Decl Name
        | Use Name
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

showIt (Decl s) = "Decl " ++ s
showIt (Use s) = "Use " ++ s
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



--program = [Decl "y", Decl "x", Block [Decl "y", Use "y", Use "w"], Decl "x", Use "y"]
pblock = Block (ConsIts (Decl "x") (ConsIts (Use "y") (ConsIts (Use "w") (NilIts))))
program = Root $ ConsIts (Decl "y") (ConsIts (Decl "x") (ConsIts (pblock) (ConsIts (Decl "x") (ConsIts (Use "y") (NilIts)))))


--programUse = [ Use x,Use y,[ Use x,Use y,Use w ],Use z ]

blockUse = Block (ConsIts (Use "x") (ConsIts (Use "y") (ConsIts (Use "w") (NilIts))))
programUse = Root $ ConsIts (Use "x") (ConsIts (Use "y") (ConsIts (blockUse) (ConsIts (Use "z") (NilIts))))

