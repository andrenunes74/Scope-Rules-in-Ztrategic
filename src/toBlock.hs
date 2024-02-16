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
import qualified Block.Shared as B
import Block.Block_Zippers

convertRoot :: Item -> B.P
convertRoot (OpenFuncao a) = B.Root (convertFuncao a)
convertRoot (OpenIf a) = B.Root (convertIf a)
convertRoot (OpenLet a) = B.Root (convertLet a)
convertRoot (OpenWhile a) = B.Root (convertWhile a)

convertFuncao :: Funcao -> B.Its
convertFuncao (Funcao (Name a) b) = B.ConsIts (B.Use a) ((convertItems b))
convertFuncao (DefFuncao (Name a) b c) = B.ConsIts (B.Decl a) (merge (convertArgs b) (convertItems c))

convertArgs :: Items -> B.Its
convertArgs (ConsIts a b) = merge (B.ConsIts (convertArg a) B.NilIts) (convertItems b) 
convertArgs (NilIts) = B.NilIts 

convertArg :: Item -> B.It
convertArg (Arg a) = B.Decl (convertE2S a)

convertIf :: If -> B.Its
convertIf (If a b) = B.ConsIts (B.Block (merge (convertExp a) (convertItems b))) B.NilIts 

convertLet :: Let -> B.Its
convertLet (Let a b) = B.ConsIts (B.Block (merge (convertItems a) (convertExp b))) B.NilIts  

convertWhile :: While -> B.Its
convertWhile (While a b) =  B.ConsIts (B.Block (merge (convertExp a) (convertItems b))) B.NilIts 

convertE2S :: Exp -> String
convertE2S (Var a) = a
convertE2S _ = "error"

convertExp :: Exp -> B.Its
convertExp (Not a) = (convertExp a)
convertExp (Inc a) = (convertExp a)
convertExp (Dec a) = (convertExp a)
convertExp (Return a) = (convertExp a)
convertExp (Add a b) = merge (convertExp a) (convertExp b)
convertExp (Sub a b) = merge (convertExp a) (convertExp b)
convertExp (Div a b) = merge (convertExp a) (convertExp b)
convertExp (Mul a b) = merge (convertExp a) (convertExp b)
convertExp (Less a b) = merge (convertExp a) (convertExp b)
convertExp (Greater a b) = merge (convertExp a) (convertExp b)
convertExp (Equals a b) = merge (convertExp a) (convertExp b)
convertExp (GTE a b) = merge (convertExp a) (convertExp b)
convertExp (LTE a b) = merge (convertExp a) (convertExp b)
convertExp (And a b) = merge (convertExp a) (convertExp b)
convertExp (Or a b) = merge (convertExp a) (convertExp b)
convertExp (Const a) = B.ConsIts (B.Use "NUMBER") B.NilIts
convertExp (Bool a) = B.ConsIts (B.Use "BOOLEAN") B.NilIts
convertExp (Var a) = B.ConsIts (B.Use (convertE2S (Var a))) B.NilIts

convertItems :: Items -> B.Its
convertItems (ConsIts a b) = merge (convertItem a) (convertItems b) 
convertItems (NilIts) = B.NilIts  

convertItem :: Item -> B.Its
convertItem (Decl a b) = B.ConsIts (B.Decl a) (convertExp b)
convertItem (Arg a) = (convertExp a)
convertItem (Increment a) = (convertExp a)
convertItem (Decrement a) = (convertExp a) 
convertItem (NestedIf a) = convertIf a
convertItem (NestedWhile a) = convertWhile a
convertItem (NestedLet a) = convertLet a
convertItem (NestedFuncao a) = convertFuncao a
convertItem (NestedReturn a) = (convertExp a)

merge :: B.Its -> B.Its -> B.Its
merge (B.NilIts) its = its
merge (B.ConsIts x xs) its = B.ConsIts x (merge xs its)

removeOccurrences :: [String]  -> [String]
removeOccurrences fromList = filter (\s -> not (s `elem` ["NUMBER","BOOLEAN"])) fromList

main :: Item -> Errors
main a = removeOccurrences $ block $ convertRoot a

