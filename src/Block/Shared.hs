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

type Env    = [(Name, Int, It)]
type Errors = [(Name, It, String)]

mustBeIn :: Name -> It -> Env -> Errors
mustBeIn n i e = if null (filter ((== n) . fst3) e) then [(n, i, " <= [Undeclared use!]")] else []

mustNotBeIn :: (Name, Int) -> It -> Env -> Errors
mustNotBeIn p@(name, _) i e = if p `elem` map (\(name', _, _) -> (name', 0)) e then [(name, i, " <= [Duplicated declaration!]")] else []

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x


