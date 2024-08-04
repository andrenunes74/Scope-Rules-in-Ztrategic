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
                CHsConDecl -> True -- declaration of a constructor
                CHsRecDecl -> True -- declaration of a constructor
                _ -> False
    isUse t = case constructor t of 
                CHsVar    -> True 
                CHsVarOp  -> True -- infix operator
                CHsQVarOp -> case lexeme_HsQVarOp t of -- infix operator
                    Special _ -> False 
                    _         -> True  
                CHsConOp  -> True -- infix operator
                CHsQConOp -> case lexeme_HsQConOp t of -- infix operator
                    Special _ -> False 
                    _         -> True 
                CHsPApp      -> True -- Constructor application
                CHsPInfixApp -> True -- Constructor application, but infix
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
                    Qual _ (HsIdent s)  -> s
                    Qual _ (HsSymbol s) -> s
                    UnQual (HsIdent s)  -> s
                    UnQual (HsSymbol s) -> s
                -- here downwards is infixes
                CHsVarOp  -> case lexeme_HsVarOp t of 
                    HsIdent s  -> s
                    HsSymbol s -> s 
                CHsQVarOp -> case lexeme_HsQVarOp t of 
                    Qual _ (HsIdent s)  -> s
                    Qual _ (HsSymbol s) -> s
                    UnQual (HsIdent s)  -> s
                    UnQual (HsSymbol s) -> s
                    -- "Special" case is always a pre-defined symbol
                CHsConOp  -> case lexeme_HsConOp t of 
                    HsIdent s  -> s
                    HsSymbol s -> s 
                CHsQConOp -> case lexeme_HsQConOp t of 
                    Qual _ (HsIdent s)  -> s
                    Qual _ (HsSymbol s) -> s
                    UnQual (HsIdent s)  -> s
                    UnQual (HsSymbol s) -> s
                    -- "Special" case is always a pre-defined symbol
                CHsPApp -> case lexeme_HsPApp t of 
                    Qual _ (HsIdent s)  -> s
                    Qual _ (HsSymbol s) -> s
                    UnQual (HsIdent s)  -> s
                    UnQual (HsSymbol s) -> s
                    -- "Special" case is always a pre-defined symbol
                CHsPInfixApp -> case lexeme_HsPInfixApp_2 t of 
                    Qual _ (HsIdent s)  -> s
                    Qual _ (HsSymbol s) -> s
                    UnQual (HsIdent s)  -> s
                    UnQual (HsSymbol s) -> s
                    -- "Special" case is always a pre-defined symbol
    getDecl t = case constructor t of 
                CHsPVar -> case lexeme_HsPVar t of 
                                HsIdent s -> s 
                CHsConDecl -> case lexeme_HsConDecl_2 t of 
                                HsIdent s  -> s
                                HsSymbol s -> s 
                CHsRecDecl -> case lexeme_HsRecDecl_2 t of 
                                HsIdent s  -> s
                                HsSymbol s -> s 
    setUse t s = case constructor t of
                CHsVar -> case lexeme_HsVar t of 
                            Qual x (HsIdent  a) -> setHole (HsVar (Qual x (HsIdent  $ a++s))) t
                            Qual x (HsSymbol a) -> setHole (HsVar (Qual x (HsSymbol $ a++s))) t
                            UnQual (HsIdent  a) -> setHole (HsVar (UnQual (HsIdent  $ a++s))) t
                            UnQual (HsSymbol a) -> setHole (HsVar (UnQual (HsSymbol $ a++s))) t
                CHsVarOp  -> case lexeme_HsVarOp t of
                            HsIdent  a -> setHole (HsVarOp (HsIdent  $ a++s)) t 
                            HsSymbol a -> setHole (HsVarOp (HsSymbol $ a++s)) t   
                CHsQVarOp -> case lexeme_HsQVarOp t of -- infix operator
                            Qual x (HsIdent  a) -> setHole (HsQVarOp (Qual x (HsIdent  $ a++s))) t
                            Qual x (HsSymbol a) -> setHole (HsQVarOp (Qual x (HsSymbol $ a++s))) t
                            UnQual (HsIdent  a) -> setHole (HsQVarOp (UnQual (HsIdent  $ a++s))) t
                            UnQual (HsSymbol a) -> setHole (HsQVarOp (UnQual (HsSymbol $ a++s))) t
                CHsConOp  -> case lexeme_HsConOp t of -- infix operator
                            HsIdent  a -> setHole (HsConOp (HsIdent  $ a++s)) t 
                            HsSymbol a -> setHole (HsConOp (HsSymbol $ a++s)) t
                CHsQConOp -> case lexeme_HsQConOp t of -- infix operator
                            Qual x (HsIdent  a) -> setHole (HsQConOp (Qual x (HsIdent  $ a++s))) t
                            Qual x (HsSymbol a) -> setHole (HsQConOp (Qual x (HsSymbol $ a++s))) t
                            UnQual (HsIdent  a) -> setHole (HsQConOp (UnQual (HsIdent  $ a++s))) t
                            UnQual (HsSymbol a) -> setHole (HsQConOp (UnQual (HsSymbol $ a++s))) t
                CHsPApp -> case lexeme_HsPApp t of -- application (of a constructor?)
                            Qual x (HsIdent  a) -> setHole (HsPApp (Qual x (HsIdent  $ a++s)) (lexeme_HsPApp_2 t)) t
                            Qual x (HsSymbol a) -> setHole (HsPApp (Qual x (HsSymbol $ a++s)) (lexeme_HsPApp_2 t)) t
                            UnQual (HsIdent  a) -> setHole (HsPApp (UnQual (HsIdent  $ a++s)) (lexeme_HsPApp_2 t)) t
                            UnQual (HsSymbol a) -> setHole (HsPApp (UnQual (HsSymbol $ a++s)) (lexeme_HsPApp_2 t)) t
                CHsPInfixApp -> case lexeme_HsPInfixApp_2 t of -- infix application (of a constructor?)
                            Qual x (HsIdent  a) -> setHole (HsPInfixApp (lexeme_HsPInfixApp t) (Qual x (HsIdent  $ a++s)) (lexeme_HsPInfixApp_3 t)) t
                            Qual x (HsSymbol a) -> setHole (HsPInfixApp (lexeme_HsPInfixApp t) (Qual x (HsSymbol $ a++s)) (lexeme_HsPInfixApp_3 t)) t
                            UnQual (HsIdent  a) -> setHole (HsPInfixApp (lexeme_HsPInfixApp t) (UnQual (HsIdent  $ a++s)) (lexeme_HsPInfixApp_3 t)) t
                            UnQual (HsSymbol a) -> setHole (HsPInfixApp (lexeme_HsPInfixApp t) (UnQual (HsIdent  $ a++s)) (lexeme_HsPInfixApp_3 t)) t
    setDecl t s = case constructor t of 
                    CHsPVar -> case lexeme_HsPVar t of 
                        HsIdent  a -> setHole (HsPVar $ HsIdent  $ a++s) t 
                        HsSymbol a -> setHole (HsPVar $ HsSymbol $ a++s) t  
                    CHsConDecl -> case lexeme_HsConDecl_2 t of 
                        HsIdent  a -> setHole (HsConDecl (lexeme_HsConDecl t) (HsIdent  $ a++s) (lexeme_HsConDecl_3 t)) t 
                        HsSymbol a -> setHole (HsConDecl (lexeme_HsConDecl t) (HsSymbol $ a++s) (lexeme_HsConDecl_3 t)) t  
                    CHsRecDecl -> case lexeme_HsRecDecl_2 t of 
                        HsIdent  a -> setHole (HsRecDecl (lexeme_HsRecDecl t) (HsIdent  $ a++s) (lexeme_HsRecDecl_3 t)) t 
                        HsSymbol a -> setHole (HsRecDecl (lexeme_HsRecDecl t) (HsSymbol $ a++s) (lexeme_HsRecDecl_3 t)) t  
                    
    initialState = const ["error", "show", "$"]


-- TODO: constructor usage, if inside "data X =" then its decl, else its use. Check "ParseOK" in example2 
-- ^ use upwards to check if it's inside a "data=" definition <- no need, as theres a constructor just for that
-- ^^ done, without upwards
-- TODO: add infix verification (1 `otherFunc` 2)  <--- DONE / UPD: not done! distinguish decls and uses?!
-- TODO: check if we need more of HsDecls as declarations?
-- TODO: refactor - make HsQName -> String and use it! 

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
\data Parse = ParseOke String \n\
\parseExample = case parseModule example2 of \n\
\                ParseOk x -> x `asdf` y \n\
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