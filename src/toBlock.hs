{-# LANGUAGE DeriveDataTypeable #-}

module Block where
import Core
import Scopes
import Data.Data
import Data.Generics.Zipper
import Library.StrategicData (StrategicData)
import Library.Ztrategic
import Data.Generics.Aliases
import Library.ZipperAG
import Data.Maybe
import Debug.Trace

data P  = Root Its
          deriving (Typeable, Data,Eq)

data Its = ConsIt It Its
         | Nil
       deriving (Typeable, Data,Eq)

data It = Assign N
        | Use N
        | Block Its
        deriving (Typeable, Data,Eq)

type N = String

instance Show P where
   show = showRoot

showRoot (Root its) = "[ " ++ showIts its ++ " ]\n"

instance Show Its where
   show = showIts

showIts (ConsIt it Nil) = show it
showIts (ConsIt it its)    = show it ++ "," ++ showIts its
showIts Nil              = ""

instance Show It where
  show = showIt

showIt (Assign s) = "Assign " ++ s
showIt (Use s) = "Use " ++ s
showIt (Block its) = "[ " ++ showIts its ++ " ]"

convertRoot :: Item -> P
convertRoot (OpenFuncao a) = Root (convertFuncao a)
convertRoot (OpenIf a) = Root (convertIf a)
convertRoot (OpenLet a) = Root (convertLet a)
convertRoot (OpenWhile a) = Root (convertWhile a)

convertFuncao :: Funcao -> Its
convertFuncao (Funcao (Name a) b) = ConsIt (Assign a) ((convertItems b))
convertFuncao (DefFuncao (Name a) b c) = ConsIt (Assign a) (merge (convertItems b) (convertItems c))

convertIf :: If -> Its
convertIf (If a b) = ConsIt (Block (ConsIt (convertExp a) (convertItems b))) Nil

convertLet :: Let -> Its
convertLet (Let a b) = ConsIt (Block (ConsIt (convertExp b) (convertItems a))) Nil 

convertWhile :: While -> Its
convertWhile (While a b) =  ConsIt (Block (ConsIt (convertExp a) (convertItems b))) Nil

convertE2S :: Exp -> String
convertE2S (Var a) = a
convertE2S _ = "error"

convertExp :: Exp -> It
convertExp (Not a) = convertExp a
convertExp (Inc a) = convertExp a
convertExp (Dec a) = convertExp a
convertExp (Return a) = convertExp a
convertExp (Add a b) = Block (ConsIt (convertExp a) (ConsIt (convertExp b) Nil))
convertExp (Sub a b) = Block (ConsIt (convertExp a) (ConsIt (convertExp b) Nil))
convertExp (Div a b) = Block (ConsIt (convertExp a) (ConsIt (convertExp b) Nil))
convertExp (Mul a b) = Block (ConsIt (convertExp a) (ConsIt (convertExp b) Nil))
convertExp (Less a b) = Block (ConsIt (convertExp a) (ConsIt (convertExp b) Nil))
convertExp (Greater a b) = Block (ConsIt (convertExp a) (ConsIt (convertExp b) Nil))
convertExp (Equals a b) = Block (ConsIt (convertExp a) (ConsIt (convertExp b) Nil))
convertExp (GTE a b) = Block (ConsIt (convertExp a) (ConsIt (convertExp b) Nil))
convertExp (LTE a b) = Block (ConsIt (convertExp a) (ConsIt (convertExp b) Nil))
convertExp (And a b) = Block (ConsIt (convertExp a) (ConsIt (convertExp b) Nil))
convertExp (Or a b) = Block (ConsIt (convertExp a) (ConsIt (convertExp b) Nil))
convertExp (Const a) = Use "NUMBER"
convertExp (Bool a) = Use "BOOLEAN"
convertExp (Var a) = Use (convertE2S (Var a))

convertItems :: Items -> Its
convertItems (ConsIts a b) = ConsIt (convertIt a) (convertItems b)
convertItems (NilIts) = Nil 

convertIt :: Item -> It
convertIt (Decl a b) = Block (ConsIt (Assign a) (ConsIt (convertExp b) Nil))
convertIt (Arg a) = Use (convertE2S a)
convertIt (Increment a) = Use (convertE2S a)
convertIt (Decrement a) = Use (convertE2S a)
convertIt (NestedIf a) = Block (convertIf a)
convertIt (NestedFuncao a) = Block (convertFuncao a)
convertIt (NestedWhile a) = Block (convertWhile a)
convertIt (NestedLet a) = Block (convertLet a)
convertIt (NestedReturn a) = Block (ConsIt (convertExp a) Nil)

convertItem :: Item -> Its
convertItem (Decl a b) = ConsIt (Assign a) (ConsIt (convertExp b) Nil)
convertItem (Arg a) = ConsIt (convertExp a) Nil
convertItem (Increment a) = ConsIt (convertExp a) Nil
convertItem (Decrement a) = ConsIt (convertExp a) Nil
convertItem (NestedIf a) = convertIf a
convertItem (NestedWhile a) = convertWhile a
convertItem (NestedLet a) = convertLet a
convertItem (NestedFuncao a) = convertFuncao a
convertItem (NestedReturn a) = ConsIt (convertIt (NestedReturn a)) Nil

merge :: Its -> Its -> Its
merge (Nil) its = its
merge (ConsIt x xs) its = ConsIt x (merge xs its)