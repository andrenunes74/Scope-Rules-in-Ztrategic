{-# LANGUAGE TemplateHaskell #-}
module Haskell_Interface where 

import Language.Haskell.Syntax
import Language.Haskell.Parser
import Language.Haskell.Pretty

import Data.Generics.Zipper
import AGMaker.AGMaker 
import IScopes

-- import GeneratedAG
import Data.Maybe (isJust)

makeAG ''HsModule
-- writeAG ''HsModule 

instance StrategicData HsModule where 
    isTerminal t = isJust (getHole t :: Maybe SrcLoc)

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
    getUse t = case constructor t of
                CHsVar -> case lexeme_HsVar t of 
                    Qual _ (HsIdent s) -> s
                    UnQual (HsIdent s) -> s 
    getDecl t = case constructor t of 
                CHsPVar -> case lexeme_HsPVar t of 
                    HsIdent s -> s 
    setUse t s = case lexeme_HsVar t of 
                    Qual x (HsIdent a) -> setHole (HsVar (Qual x (HsIdent $ a++s))) t
                    UnQual (HsIdent a) -> setHole (HsVar (UnQual (HsIdent $ a++s))) t
    setDecl t s = case lexeme_HsPVar t of 
                    HsIdent a -> setHole ( HsIdent $ a++s) t  
    initialState = const ["error", "show"]


-- TODO: constructor usage, if inside "data X =" then its decl, else its use. Check "ParseOK" in example2 
-- ^ use upwards to check if it's inside a "data=" definition
-- TODO: add infix verification (1 `otherFunc` 2)

-----
----- Quick Test
-----

parseExample  = scopesExample example1
parseExample2 = scopesExample example2


-----
----- Auxiliary
-----
scopesExample :: String -> String 
scopesExample = prettyPrint . applyErrors_a68 . parse 

errorsExample :: String -> Errors
errorsExample = processor_a68 . parse

blockExample :: String -> P
blockExample = toBlock . parse 

parse :: String -> HsModule
parse s = case parseModule s of 
                ParseOk x -> x 
                err -> error $ show err

-----
----- Full example
-----

example1_ = putStrLn example1
example1 = "\
\module Example where \n\
\f1 = let x = 1 `otherFunc` 1 \n\
\         y = 2 + x \n\
\         z = x \n\
\     in z\
\"

f1 = let x = 1 + 1
         y = 2 + x 
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

example3 = "\
\module Example where \n\
\f1 = let y = let x = 4 \n\
\                 y = 7 \n\
\             in y + z \n\
\         x = 1 + 1 \n\
\         z = x \n\
\     in z\
\"

f2 = let y = let x = 4 
                 y = 7 
             in y + z
         x = 1 + 1
         z = x 
     in z