{-# LANGUAGE TemplateHaskell #-}
module Haskell_Interface where 

import Language.Haskell.Syntax
import Language.Haskell.Parser
import Data.Generics.Zipper

import Library.StrategicData (StrategicData(..))
import AGMaker.AGMaker 

import IScopes
import Block.Shared
import Block.Block_Zippers

-- import GeneratedAG
import Data.Maybe (isJust)

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
    isBlock t = case constructor t of 
                -- let expression
                CHsLet          -> True
                -- any right-hand side. This covers functions 
                CHsGuardedRhss  -> True
                CHsUnGuardedRhs -> True
                -- patterns in a case expression
                CHsAlt          -> True
                _ -> False  
    isGlobal _ = False 

useToStr :: Zipper a -> String 
useToStr t = case constructor t of
                CHsVar -> case lexeme_HsVar t of 
                    Qual _ (HsIdent s) -> s
                    UnQual (HsIdent s) -> s 

declToStr :: Zipper a -> String 
declToStr t = case constructor t of 
                CHsPVar -> case lexeme_HsPVar t of 
                    HsIdent s -> s 


build :: Scopes a => Zipper a -> P
build a = Root (buildChildren build' a [])

build' :: Scopes a => Zipper a -> Directions -> Its
build' a d | isDecl a = ConsIts (Decl (declToStr a) d) (buildChildren build' a d)
           | isUse a = ConsIts (Use (useToStr a) d) (buildChildren build' a d)
           | isBlock a = ConsIts (Block $ buildChildren build' a d) NilIts
           | otherwise = buildChildren build' a d


-----
-----
-----

errorsExample :: String -> Errors 
errorsExample = block . runExample

runExample :: String -> P
runExample = build . toZipper . parse  

parse :: String -> HsModule
parse s = case parseModule s of 
                ParseOk x -> x 
                err -> error $ show err


parseExample  = runExample example1
parseExample2 = runExample example2

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

-- try adding Prelude directly so that build+env can find it
example2 = "\
\module Example where \n\
\import Language.Haskell.Parser \n\
\parseExample = case parseModule example2 of \n\
\                ParseOk x -> x \n\
\                err -> error $ show err\n\
\"