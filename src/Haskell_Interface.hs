{-# LANGUAGE TemplateHaskell #-}
module Haskell_Interface where 

import Language.Haskell.Syntax
import Language.Haskell.Parser
import Data.Generics.Zipper

import Library.StrategicData (StrategicData(..))
import AGMaker.AGMaker 
import Library.ZipperAG

import IScopes
import Block.Shared

-- import GeneratedAG
import Data.Maybe (isJust)

import Data.Data


-- makeAG ''HsModule
writeAG ''HsModule 

instance StrategicData HsModule where 
    isTerminal t = isJust (getHole t :: Maybe Int)
                || isJust (getHole t :: Maybe String)

instance Scopes HsModule where 
    isDecl t = case constructor t of 
                CHsPVar -> True
                _ -> False
    isUse t = case constructor t of 
                CHsVar -> True 
                _ -> False   
    isBlock _ = False 
    isGlobal _ = False 


build :: Scopes a => Zipper a -> P
build a = Root (buildChildren build' a [])

build' :: Scopes a => Zipper a -> Directions -> Its
build' a d | isDecl a = ConsIts (Decl (declToStr a) d) (buildChildren build' a d)
           | isUse a = ConsIts (Use (useToStr a) d) (buildChildren build' a d)
           | isBlock a = ConsIts (Block $ buildChildren build' a d) NilIts
           | otherwise = buildChildren build' a d

useToStr :: Zipper a -> String 
useToStr t = case constructor t of 
                CHsVar -> case lexeme_HsVar t of 
                    Qual _ (HsIdent s) -> s
                    UnQual (HsIdent s) -> s 

declToStr :: Zipper a -> String 
declToStr t = case constructor t of 
                CHsPVar -> case lexeme_HsPVar t of 
                    HsIdent s -> s 

-----
-----
-----



parseExample = case parseModule example1 of 
                ParseOk x -> x 
                err -> error $ show err

example1_ = putStrLn example1
example1 = "\
\module Example where \n\
\f1 = let x = 1 \n\
\         y = 2 \n\
\         z = x \n\
\     in z\
\"

f1 = let x = 1 
         y = 2 
         z = x 
     in z