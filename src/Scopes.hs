{-# LANGUAGE DeriveDataTypeable #-}

module Scopes where
import Core
import Data.Data
import Data.Generics.Zipper
import Library.StrategicData (StrategicData)
import Library.Ztrategic
import Data.Generics.Aliases
import Library.ZipperAG
import Data.Maybe
import Debug.Trace

treeT1 = OpenFuncao (DefFuncao (Name "main") NilIts
            (ConsIts (Decl "a" (Const 100))
            (ConsIts (Decl "x" (Add (Var "a") (Var "b"))) NilIts)))

treeT2 = OpenFuncao (DefFuncao (Name "main") NilIts
            (ConsIts (Decl "a" (Const 100))
            (ConsIts (Decl "x" (Var "a")) NilIts)))

type Env    = [(String, Int)]
type Errors = [String]
type AGTree a  = Zipper a -> a

data Constructor = CAdd
                 | CSub
                 | CDiv
                 | CMul 
                 | CLess 
                 | CGreater 
                 | CEquals 
                 | CGTE 
                 | CLTE 
                 | CAnd 
                 | COr 
                 | CNot 
                 | CConst 
                 | CVar 
                 | CInc 
                 | CDec 
                 | CReturn 
                 | CBool 
                 | CDecl 
                 | CArg 
                 | CIncrement 
                 | CDecrement 
                 | CNestedIf 
                 | COpenIf
                 | CNestedWhile 
                 | COpenWhile 
                 | CNestedLet 
                 | COpenLet 
                 | CNestedFuncao 
                 | COpenFuncao 
                 | CNestedReturn 
                 | CLet
                 | CFuncao
                 | CDefFuncao
                 | CName
                 | CIf
                 | CElse
                 | CWhile
                 | CConsIts
                 | CNilIts
                 deriving Show


constructor :: (Typeable a) => Zipper a -> Constructor
constructor a =  case (getHole a :: Maybe Exp) of
    Just (Add _ _)        -> CAdd
    Just (Sub _ _)        -> CSub
    Just (Div _ _)        -> CDiv
    Just (Mul _ _)        -> CMul
    Just (Less _ _)       -> CLess
    Just (Greater _ _)    -> CGreater
    Just (Equals _ _)     -> CEquals
    Just (GTE _ _)        -> CGTE
    Just (LTE _ _)        -> CLTE
    Just (And _ _)        -> CAnd
    Just (Or _ _)         -> COr
    Just (Not _)          -> CNot
    Just (Const _)        -> CConst
    Just (Var _)          -> CVar
    Just (Inc _)          -> CInc
    Just (Dec _)          -> CDec
    Just (Return _)       -> CReturn
    Just (Bool _)         -> CBool
    otherwise             -> case (getHole a :: Maybe Item) of
        Just (Decl _ _)         -> CDecl
        Just (Arg _)            -> CArg
        Just (Increment _)      -> CIncrement
        Just (Decrement _)      -> CDecrement
        Just (NestedIf _)       -> CNestedIf
        Just (OpenIf _)         -> COpenIf
        Just (NestedWhile _)    -> CNestedWhile
        Just (OpenWhile _)      -> COpenWhile
        Just (NestedLet _)      -> CNestedLet
        Just (OpenLet _)        -> COpenLet
        Just (NestedFuncao _)   -> CNestedFuncao
        Just (OpenFuncao _)     -> COpenFuncao
        Just (NestedReturn _)   -> CNestedReturn
        otherwise               -> case (getHole a :: Maybe Let) of
            Just (Let _ _)        -> CLet
            otherwise             -> case (getHole a :: Maybe Funcao) of
                Just (Funcao _ _)         -> CFuncao
                Just (DefFuncao _ _ _)    -> CDefFuncao
                otherwise                 -> case (getHole a :: Maybe Name) of
                    Just (Name _)            -> CName
                    otherwise              -> case (getHole a :: Maybe If) of
                        Just (If _ _)              -> CIf
                        Just (Else _ _ _)          -> CElse
                        otherwise                 -> case (getHole a :: Maybe While) of
                            Just (While _ _)         -> CWhile
                            otherwise                 -> case (getHole a :: Maybe Items) of
                                Just (ConsIts _ _)    -> CConsIts
                                Just (NilIts)         -> CNilIts
                                otherwise                -> error "Naa, that production does not exist!"


lev :: (Typeable a) => Zipper a -> Int
lev ag =  case (constructor ag) of
            CAdd -> lev (parent ag)
            CSub -> lev (parent ag)
            CDiv -> lev (parent ag)
            CMul -> lev (parent ag)
            CLess -> lev (parent ag)
            CGreater -> lev (parent ag)
            CEquals -> lev (parent ag)
            CGTE -> lev (parent ag)
            CLTE -> lev (parent ag)
            CAnd -> lev (parent ag)
            COr -> lev (parent ag)
            CNot -> lev (parent ag)
            CConst -> lev (parent ag)
            CVar -> lev (parent ag)
            CInc -> lev (parent ag)
            CDec -> lev (parent ag) 
            CReturn -> lev (parent ag)
            CBool -> lev (parent ag)
            CDecl -> lev (parent ag)
            CArg -> lev (parent ag)
            CIncrement -> lev (parent ag)
            CDecrement -> lev (parent ag)
            CNestedIf -> 1 + lev (parent ag)
            CNestedWhile -> 1 + lev (parent ag)
            CNestedLet -> 1 + lev (parent ag)
            CNestedFuncao -> 1 + lev (parent ag)
            CNestedReturn -> 1 + lev (parent ag)
            CLet -> 1 + lev (parent ag)
            CFuncao -> 1 + lev (parent ag)
            CDefFuncao -> 1 + lev (parent ag)
            CName -> lev (parent ag)
            CIf -> 1 + lev (parent ag)
            CElse -> 1 + lev (parent ag)
            CWhile -> 1 + lev (parent ag)
            COpenFuncao -> 0
            COpenIf -> 0
            COpenLet -> 0
            COpenWhile -> 0
            CConsIts -> lev (parent ag)
            CNilIts -> 1 + lev (parent ag)

dcli :: Typeable a => Zipper a -> Env
dcli t =  case constructor t of
            CAdd -> dcli (parent t)
            CSub -> dcli (parent t)
            CDiv -> dcli (parent t)
            CMul -> dcli (parent t)
            CLess -> dcli (parent t)
            CGreater -> dcli (parent t)
            CEquals -> dcli (parent t)
            CGTE -> dcli (parent t)
            CLTE -> dcli (parent t)
            CAnd -> dcli (parent t)
            COr -> dcli (parent t)
            CNot -> dcli (parent t)
            CConst -> dcli (parent t)
            CVar -> dcli (parent t)
            CInc -> dcli (parent t)
            CDec -> dcli (parent t)
            CReturn -> dcli (parent t)
            CBool -> dcli (parent t)
            CDecl -> dcli (parent t)
            CArg -> dcli (parent t)
            CIncrement -> dcli (parent t)
            CDecrement -> dcli (parent t)
            CConsIts -> dcli (parent t)
            CNilIts -> dcli (parent t)
            CNestedIf -> case  (constructor $ parent t) of
                            CLet    -> env (parent t)
                            CDefFuncao    -> env (parent t)
                            CIf -> env (parent t)
                            CElse -> env (parent t)
                            CWhile -> env (parent t)
                            CNestedWhile -> env (parent t)
                            CNestedIf -> env (parent t)
                            CNestedFuncao -> env (parent t)
                            CNestedLet -> env (parent t)
                            CNestedReturn -> env (parent t)
                            COpenFuncao -> []
                            COpenIf -> []
                            COpenLet -> []
                            COpenWhile -> []
                            otherwise -> dclo (t.$<1)
            CNestedWhile -> case  (constructor $ parent t) of
                            CLet    -> env (parent t)
                            CDefFuncao    -> env (parent t)
                            CIf -> env (parent t)
                            CElse -> env (parent t)
                            CWhile -> env (parent t)
                            CNestedWhile -> env (parent t)
                            CNestedIf -> env (parent t)
                            CNestedFuncao -> env (parent t)
                            CNestedLet -> env (parent t)
                            CNestedReturn -> env (parent t)
                            COpenFuncao -> []
                            COpenIf -> []
                            COpenLet -> []
                            COpenWhile -> []
                            otherwise -> dclo (t.$<1)
            CNestedLet -> case  (constructor $ parent t) of
                            CLet    -> env (parent t)
                            CDefFuncao    -> env (parent t)
                            CIf -> env (parent t)
                            CElse -> env (parent t)
                            CWhile -> env (parent t)
                            CNestedWhile -> env (parent t)
                            CNestedIf -> env (parent t)
                            CNestedFuncao -> env (parent t)
                            CNestedLet -> env (parent t)
                            CNestedReturn -> env (parent t)
                            COpenFuncao -> []
                            COpenIf -> []
                            COpenLet -> []
                            COpenWhile -> []
                            otherwise -> dclo (t.$<1)
            CNestedFuncao -> case  (constructor $ parent t) of
                            CLet    -> env (parent t)
                            CDefFuncao    -> env (parent t)
                            CIf -> env (parent t)
                            CElse -> env (parent t)
                            CWhile -> env (parent t)
                            CNestedWhile -> env (parent t)
                            CNestedIf -> env (parent t)
                            CNestedFuncao -> env (parent t)
                            CNestedLet -> env (parent t)
                            CNestedReturn -> env (parent t)
                            COpenFuncao -> []
                            COpenIf -> []
                            COpenLet -> []
                            COpenWhile -> []
                            otherwise -> dclo (t.$<1)
            CNestedReturn -> case  (constructor $ parent t) of
                            CLet    -> env (parent t)
                            CDefFuncao    -> env (parent t)
                            CIf -> env (parent t)
                            CElse -> env (parent t)
                            CWhile -> env (parent t)
                            CNestedWhile -> env (parent t)
                            CNestedIf -> env (parent t)
                            CNestedFuncao -> env (parent t)
                            CNestedLet -> env (parent t)
                            CNestedReturn -> env (parent t)
                            COpenFuncao -> []
                            COpenIf -> []
                            COpenLet -> []
                            COpenWhile -> []
                            otherwise -> dclo (t.$<1)
            CLet -> case  (constructor $ parent t) of
                            CLet    -> env (parent t)
                            CDefFuncao    -> env (parent t)
                            CIf -> env (parent t)
                            CElse -> env (parent t)
                            CWhile -> env (parent t)
                            CNestedWhile -> env (parent t)
                            CNestedIf -> env (parent t)
                            CNestedFuncao -> env (parent t)
                            CNestedLet -> env (parent t)
                            CNestedReturn -> env (parent t)
                            COpenFuncao -> []
                            COpenIf -> []
                            COpenLet -> []
                            COpenWhile -> []
                            otherwise -> dclo (t.$<1)
            CFuncao -> case  (constructor $ parent t) of
                            CLet    -> env (parent t)
                            CDefFuncao    -> env (parent t)
                            CIf -> env (parent t)
                            CElse -> env (parent t)
                            CWhile -> env (parent t)
                            CNestedWhile -> env (parent t)
                            CNestedIf -> env (parent t)
                            CNestedFuncao -> env (parent t)
                            CNestedLet -> env (parent t)
                            CNestedReturn -> env (parent t)
                            COpenFuncao -> []
                            COpenIf -> []
                            COpenLet -> []
                            COpenWhile -> []
                            otherwise -> dclo (t.$<1)
            CDefFuncao -> case  (constructor $ parent t) of
                            CLet    -> env (parent t)
                            CDefFuncao    -> env (parent t)
                            CIf -> env (parent t)
                            CElse -> env (parent t)
                            CWhile -> env (parent t)
                            CNestedWhile -> env (parent t)
                            CNestedIf -> env (parent t)
                            CNestedFuncao -> env (parent t)
                            CNestedLet -> env (parent t)
                            CNestedReturn -> env (parent t)
                            COpenFuncao -> []
                            COpenIf -> []
                            COpenLet -> []
                            COpenWhile -> []
                            otherwise -> dclo (t.$<1)
            CIf -> case  (constructor $ parent t) of
                            CLet    -> env (parent t)
                            CDefFuncao    -> env (parent t)
                            CIf -> env (parent t)
                            CElse -> env (parent t)
                            CWhile -> env (parent t)
                            CNestedWhile -> env (parent t)
                            CNestedIf -> env (parent t)
                            CNestedFuncao -> env (parent t)
                            CNestedLet -> env (parent t)
                            CNestedReturn -> env (parent t)
                            COpenFuncao -> []
                            COpenIf -> []
                            COpenLet -> []
                            COpenWhile -> []
                            otherwise -> dclo (t.$<1)
            CElse -> case  (constructor $ parent t) of
                            CLet    -> env (parent t)
                            CDefFuncao    -> env (parent t)
                            CIf -> env (parent t)
                            CElse -> env (parent t)
                            CWhile -> env (parent t)
                            CNestedWhile -> env (parent t)
                            CNestedIf -> env (parent t)
                            CNestedFuncao -> env (parent t)
                            CNestedLet -> env (parent t)
                            CNestedReturn -> env (parent t)
                            COpenFuncao -> []
                            COpenIf -> []
                            COpenLet -> []
                            COpenWhile -> []
                            otherwise -> dclo (t.$<1)
            CWhile -> case  (constructor $ parent t) of
                            CLet    -> env (parent t)
                            CDefFuncao    -> env (parent t)
                            CIf -> env (parent t)
                            CElse -> env (parent t)
                            CWhile -> env (parent t)
                            CNestedWhile -> env (parent t)
                            CNestedIf -> env (parent t)
                            CNestedFuncao -> env (parent t)
                            CNestedLet -> env (parent t)
                            CNestedReturn -> env (parent t)
                            COpenFuncao -> []
                            COpenIf -> []
                            COpenLet -> []
                            COpenWhile -> []
                            otherwise -> dclo (t.$<1)
            CName -> dclo (t.$<1)
            COpenFuncao -> []
            COpenIf -> []
            COpenLet -> []
            COpenWhile -> []


lexeme a =  case (getHole a :: Maybe Exp) of
            Just (Var a)          -> a
            otherwise             -> case (getHole a :: Maybe Item) of
                Just (Decl a _)         -> a
                otherwise             -> case (getHole a :: Maybe Funcao) of
                    Just (Funcao (Name a) _)         -> a
                    Just (DefFuncao (Name a) _ _)    -> a
                    otherwise                 -> case (getHole a :: Maybe Name) of
                        Just (Name a)            -> a
                        otherwise                 -> error "Error in lexeme!"

dclo :: Typeable a => Zipper a -> Env
dclo t =   case constructor t of
            CDecl -> (lexeme t,lev t) : (dcli t)
            CVar -> (lexeme t,lev t) : (dcli t)
            CFuncao -> (lexeme t,lev t) : (dcli t)
            CDefFuncao -> [(lexeme t,lev t)] ++ dclo (t.$2) ++ dclo (t.$3) ++ (dcli t)
            COpenFuncao -> dclo (t.$1)
            CName -> (lexeme t,lev t) : (dcli t)
            CLet -> dclo (t.$1)
            CIf -> dclo (t.$2)
            CElse -> dclo (t.$2) ++ dclo (t.$3)
            CWhile -> dclo (t.$2)
            CConsIts -> dclo (t.$1) ++ (dclo (t.$2))
            otherwise -> dcli t

env :: Typeable a => Zipper a -> Env
env t = case constructor t of
            CAdd -> env (parent t)
            CSub -> env (parent t)
            CDiv -> env (parent t)
            CMul -> env (parent t)
            CLess -> env (parent t)
            CGreater -> env (parent t)
            CEquals -> env (parent t)
            CGTE -> env (parent t)
            CLTE -> env (parent t)
            CAnd -> env (parent t)
            COr -> env (parent t)
            CNot -> env (parent t)
            CConst -> env (parent t)
            CVar -> env (parent t)
            CInc -> env (parent t)
            CDec -> env (parent t)
            CReturn -> env (parent t)
            CBool -> env (parent t)
            CDecl -> env (parent t)
            CArg -> env (parent t)
            CIncrement -> env (parent t)
            CDecrement -> env (parent t)
            COpenFuncao -> dclo t
            COpenIf -> dclo t
            COpenLet -> dclo t
            COpenWhile -> dclo t
            CConsIts -> env (parent t)
            CNilIts -> env (parent t)
            _ -> case (constructor $ parent t) of
                    CLet    -> env (parent t)
                    CDefFuncao    -> env (parent t)
                    CIf -> env (parent t)
                    CElse -> env (parent t)
                    CWhile -> env (parent t)
                    CNestedWhile -> env (parent t)
                    CNestedIf -> env (parent t)
                    CNestedFuncao -> env (parent t)
                    CNestedLet -> env (parent t)
                    CNestedReturn -> env (parent t)
                    COpenFuncao -> env (parent t)
                    COpenIf -> env (parent t)
                    COpenLet -> env (parent t)
                    COpenWhile -> env (parent t)
                    otherwise -> dclo t

mustBeIn :: String -> Env -> Errors
mustBeIn name [] = trace (name) $ [name]
mustBeIn name ((n,i):es) = if (n==name) then [] else mustBeIn name es


mustNotBeIn :: (String, Int) -> Env -> Errors
mustNotBeIn (a,b) [] = []
mustNotBeIn (n1, l1) ((n2, l2):es) = if (n1 == n2) && (l1 == l2)
                                        then [n1] else mustNotBeIn (n1, l1) es


scopes :: Typeable a => Zipper a -> Errors
scopes ag = case (constructor ag) of
    CAdd -> (scopes (ag.$1)) ++ (scopes (ag.$2))
    CSub -> (scopes (ag.$1)) ++ (scopes (ag.$2))
    CDiv -> (scopes (ag.$1)) ++ (scopes (ag.$2))
    CMul -> (scopes (ag.$1)) ++ (scopes (ag.$2))
    CLess -> (scopes (ag.$1)) ++ (scopes (ag.$2))
    CGreater -> (scopes (ag.$1)) ++ (scopes (ag.$2))
    CEquals -> (scopes (ag.$1)) ++ (scopes (ag.$2))
    CGTE -> (scopes (ag.$1)) ++ (scopes (ag.$2))
    CLTE -> (scopes (ag.$1)) ++ (scopes (ag.$2))
    CAnd -> (scopes (ag.$1)) ++ (scopes (ag.$2))
    COr -> (scopes (ag.$1)) ++ (scopes (ag.$2))
    CNot -> mustBeIn (lexeme ag) (env ag ++ dcli ag)
    CConst -> []
    CVar -> mustBeIn (lexeme ag) (env ag ++ dcli ag)
    CInc -> mustBeIn (lexeme (ag.$1)) (env ag ++ dcli ag)
    CDec -> mustBeIn (lexeme (ag.$1)) (env ag ++ dcli ag)
    CReturn -> mustBeIn (lexeme ag) (env ag ++ dcli ag)
    CBool -> []
    CDecl -> mustNotBeIn (lexeme (ag.$1), lev ag) (dcli ag) ++ (scopes (ag.$2))
    CArg -> mustNotBeIn (lexeme ag, lev ag) (dcli ag)
    CIncrement -> mustBeIn (lexeme ag) (env ag ++ dcli ag)
    CDecrement -> mustBeIn (lexeme ag) (env ag ++ dcli ag)
    CNestedIf -> scopes (ag.$1)
    CNestedWhile -> scopes (ag.$1)
    CNestedLet -> scopes (ag.$1)
    CNestedFuncao -> scopes (ag.$1)
    CNestedReturn -> scopes (ag.$1)
    CLet -> mustBeIn (lexeme ag) (env (ag.$1) ++ dcli ag)
    CFuncao -> mustBeIn (lexeme (ag.$1)) (dcli ag) ++ (scopes (ag.$2))
    CDefFuncao -> mustNotBeIn (lexeme (ag.$1), lev ag) (dcli ag) ++ (scopes (ag.$2)) ++ (scopes (ag.$3))
    CName -> []
    CIf -> (scopes (ag.$1)) ++ (scopes (ag.$2))
    CElse -> (scopes (ag.$1)) ++ (scopes (ag.$2)) ++ (scopes (ag.$3))
    CWhile -> (scopes (ag.$1)) ++ (scopes (ag.$2))
    COpenFuncao -> scopes (ag.$1)
    COpenIf -> scopes (ag.$1)
    COpenLet -> scopes (ag.$1)
    COpenWhile -> scopes (ag.$1)
    CConsIts -> (scopes (ag.$1)) ++ (scopes (ag.$2))
    CNilIts -> []

