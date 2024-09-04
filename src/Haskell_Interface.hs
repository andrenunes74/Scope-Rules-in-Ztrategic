{-# LANGUAGE TemplateHaskell, PackageImports #-}
module Haskell_Interface where

-- import "haskell-src" Language.Haskell.Syntax
-- import qualified "ghc" Language.Haskell.Syntax as GHC

import Language.Haskell.Syntax

import Language.Haskell.Parser
import Language.Haskell.Pretty

import qualified Language.Haskell.Exts as LHE
import Library.Ztrategic

import Data.Generics.Zipper
import AGMaker.AGMaker
import IScopes

-- import GeneratedAG
import Data.Maybe (isJust)
import Language.Grammars.ZipperAG (parent)
import Library.ZipperAG ((.^^))
import GHC.IO (unsafePerformIO)

makeAG ''HsModule
-- writeAG ''HsModule 

instance StrategicData HsModule where
    isTerminal t = isJust (getHole t :: Maybe SrcLoc)

instance Scopes HsModule where
    isDecl t = case constructor t of
                CHsPVar -> True
                -- CHsPatBind -> True -- we try to fix functions
                CHsFunBind -> True -- we try to fix functions
                CHsConDecl -> True -- declaration of a constructor
                CHsRecDecl -> True -- declaration of a constructor
                -- CHsIdent -> case constructor $ parent t of -- check if we are a name of a function
                --                 CHsMatch -> True
                --                 _ -> False
                -- CHsSymbol -> case constructor $ parent t of -- check if we are a name of a function
                --                 CHsMatch -> True
                --                 _ -> False
                _ -> False
    isUse t = case constructor t of
                CHsVar    -> True
                -- CQual     -> True
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
                -- function bind
                CHsMatch        -> True
                -- any right-hand side. This covers functions 
                CHsGuardedRhss  -> True
                CHsUnGuardedRhs -> True
                -- patterns in a case expression
                CHsAlt          -> True
                _ -> False
    getUse t = case constructor t of
                CHsVar       -> hsQName2Str $ lexeme_HsVar t
                -- CQual        -> case getHole t of 
                --     Just (Qual (Module x) (HsIdent y)) -> x++"."++y
                CHsVarOp     -> hsName2Str  $ lexeme_HsVarOp t
                CHsQVarOp    -> hsQName2Str $ lexeme_HsQVarOp t
                CHsConOp     -> hsName2Str  $ lexeme_HsConOp t
                CHsQConOp    -> hsQName2Str $ lexeme_HsQConOp t
                CHsPApp      -> hsQName2Str $ lexeme_HsPApp t
                CHsPInfixApp -> hsQName2Str $ lexeme_HsPInfixApp_2 t
    getDecl t = case constructor t of
                -- CHsPatBind -> case lexeme_HsPatBind_2 t of
                --                 HsPVar (HsIdent  s) -> s
                --                 HsPVar (HsSymbol s) -> s
                                -- other cases we might need to say it's not a decl
                CHsFunBind -> case head $ lexeme_HsFunBind t of -- we only take a look at the first and we assume all are equal in name
                                HsMatch _ hsname _ _ _ -> qualify t $ hsName2Str hsname
                CHsPVar    -> qualify t $ hsName2Str $ lexeme_HsPVar t
                CHsConDecl -> qualify t $ hsName2Str $ lexeme_HsConDecl_2 t
                CHsRecDecl -> qualify t $ hsName2Str $ lexeme_HsRecDecl_2 t
                -- CHsIdent -> case constructor $ parent t of -- check if we are a name of a function
                --                 CHsMatch -> lexeme_HsIdent t
                --                 _ -> error "Unexpected state"
                -- CHsSymbol -> case constructor $ parent t of -- check if we are a name of a function
                --                 CHsMatch -> lexeme_HsSymbol t
                --                 _ -> error "Unexpected state"
    setUse t s = case constructor t of
                CHsVar       -> setHole (HsVar (appendHsQNameStr (lexeme_HsVar t) s)) t
                CHsVarOp     -> setHole (HsVarOp (appendHsNameStr (lexeme_HsVarOp t) s)) t
                CHsQVarOp    -> setHole (HsQVarOp (appendHsQNameStr (lexeme_HsQVarOp t) s)) t -- infix operator
                CHsConOp     -> setHole (HsConOp (appendHsNameStr (lexeme_HsConOp t) s)) t -- infix operator
                CHsQConOp    -> setHole (HsQConOp (appendHsQNameStr (lexeme_HsQConOp t) s)) t -- infix operator
                CHsPApp      -> setHole (HsPApp (appendHsQNameStr (lexeme_HsPApp t) s) (lexeme_HsPApp_2 t)) t -- application (of a constructor?)
                CHsPInfixApp -> setHole (HsPInfixApp (lexeme_HsPInfixApp t) (appendHsQNameStr (lexeme_HsPInfixApp_2 t) s) (lexeme_HsPInfixApp_3 t)) t
    setDecl t s = case constructor t of
                    CHsPVar -> setHole (HsPVar $ appendHsNameStr (lexeme_HsPVar t) s) t
                    CHsPatBind -> case lexeme_HsPatBind_2 t of
                        HsPVar hsname -> setHole (HsPatBind (lexeme_HsPatBind t) (HsPVar $  appendHsNameStr hsname s) (lexeme_HsPatBind_3 t) (lexeme_HsPatBind_4 t)) t
                                    -- other cases we might need to say it's not a decl
                    CHsFunBind -> case head $ lexeme_HsFunBind t of -- we only take a look at the first and we assume all are equal in name
                                    HsMatch q hsname w e r -> setHole (HsMatch q (appendHsNameStr hsname s) w e r : tail (lexeme_HsFunBind t)) t
                    CHsConDecl -> setHole (HsConDecl (lexeme_HsConDecl t) (appendHsNameStr (lexeme_HsConDecl_2 t) s) (lexeme_HsConDecl_3 t)) t
                    CHsRecDecl -> setHole (HsRecDecl (lexeme_HsRecDecl t) (appendHsNameStr (lexeme_HsRecDecl_2 t) s) (lexeme_HsRecDecl_3 t)) t

    initialState = const names -- const ["error", "show", "$"]

-- TODO: reviews on overleaf <- DONE
-- TODO: think about qualified names: for example (Qual Language.Haskell.Pretty pretty)
-- TODO: change IScopes.build to allow definition of list of names, instead of definition of just 1 name, per node
-- TODO: all decl x should declare x and also ModuleName.x (upwards until root to get module name?)

-- new problem: since we declare twice, we also find errors twice!

-- TODO: strategy to grab all names from prelude, into initialState
-- TODO: properties on Haskell? What properties? 
-- TODO: compare upwards dclblock+env vs normal env (using v)
-- TODO: big haskell file? (last resort, let generator)
-- TODO: can we include qualification into the Interface itself? Its a generic problem, not just haskell

-----
----- Helper functions
-----

hsName2Str :: HsName -> String
hsName2Str (HsIdent  s) = s
hsName2Str (HsSymbol s) = s

hsQName2Str :: HsQName -> String
hsQName2Str (Qual x hsname) = hsName2Str hsname
hsQName2Str (UnQual hsname) = hsName2Str hsname

appendHsNameStr :: HsName -> String -> HsName
appendHsNameStr (HsIdent  a) s = HsIdent  $ a++s
appendHsNameStr (HsSymbol a) s = HsSymbol $ a++s

appendHsQNameStr :: HsQName -> String -> HsQName
appendHsQNameStr (Qual x hsname) s = Qual x $ appendHsNameStr hsname s
appendHsQNameStr (UnQual hsname) s = UnQual $ appendHsNameStr hsname s

qualify :: Scopes a => Zipper a -> String -> [String]
qualify t s = [s, moduleName ++ "." ++ s]
 where (Module moduleName) = lexeme_HsModule_2 .^^ t

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
\data Example = ParseOk String | ParseOk Int \n\
\ \n\ 
\x `asdf` y = x \n\
\x `asdf` 1 = 1 \n\
\f = \\x y -> z \n\
\parseExample x = let parseModule y = Language.Haskell.Parser.foo \n\
\                     foo = parseModule 1 \n\
\                  in case parseModule example2 of \n\
\                ParseOk x -> x `asdf` y \n\
\                err -> error $ show err\n\
\ where y = 1 \n\
\        where y = 2 \n\
\"

-- where inside where, should open new scopes

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


exampleModule = "\
\module Example ( \n\
\    Bool(False, True), \n\
\    (&&), (||), not, otherwise, \n\
\ \n\
\    Maybe(Nothing, Just), \n\
\    maybe, \n\
\    Num((+), (-), (*), negate, abs, signum, fromInteger), \n\
\    Real(toRational), \n\
\ ) where \n\
\f1 = 4 \n\
\"

instance (StrategicData (LHE.Module LHE.SrcSpanInfo))

names = unsafePerformIO $ namesFromFile "./base-4.17.2.1/Prelude.hs"

namesFromFile :: FilePath -> IO [String]
namesFromFile s = do 
    r <- LHE.parseFile s
    return $ case r of 
        LHE.ParseOk t -> applyTU (full_tdTU (failTU `adhocTU` oneName)) $ toZipper t 
        _ -> ["error"] 

namesFromModule :: String -> [String]
namesFromModule s = case LHE.parseFileContents s of 
    LHE.ParseOk t -> applyTU (full_tdTU (failTU `adhocTU` oneName)) $ toZipper t 
    _ -> ["error"] 

oneName :: LHE.ExportSpec LHE.SrcSpanInfo -> [String]
oneName (LHE.EVar _ qname) = [qnameToStr qname]
oneName (LHE.EAbs _ namespace qname) = [qnameToStr qname]
oneName (LHE.EThingWith _ wildcard qname cnamelist) = qnameToStr qname : map cnameToStr cnamelist
oneName (LHE.EModuleContents _ (LHE.ModuleName _ s)) = ["Module <s>"]

qnameToStr :: LHE.QName LHE.SrcSpanInfo -> String 
qnameToStr (LHE.Qual _ moduleName name) = nameToStr name 
qnameToStr (LHE.UnQual _ name)          = nameToStr name 

cnameToStr :: LHE.CName LHE.SrcSpanInfo -> String 
cnameToStr (LHE.VarName _ name) = nameToStr name 
cnameToStr (LHE.ConName _ name) = nameToStr name 

nameToStr :: LHE.Name LHE.SrcSpanInfo -> String 
nameToStr (LHE.Ident  _ s) = s 
nameToStr (LHE.Symbol _ s) = s 
