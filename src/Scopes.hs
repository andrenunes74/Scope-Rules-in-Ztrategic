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

treeT3 = OpenFuncao (DefFuncao (Name "count25") NilIts
            (ConsIts (Decl "counter" (Const 0))
            (ConsIts (NestedWhile (While ((Less (Var "counter") (Const 5)))
                                    (ConsIts (Increment (Var "counter"))
                                    NilIts)))
            NilIts)))

treeT4 = OpenFuncao (DefFuncao (Name "count25") NilIts
            (ConsIts (Decl "counter" (Const 0))
            (ConsIts (NestedIf (If ((Less (Var "counter") (Const 5)))
                                    (ConsIts (NestedReturn (Return (Var "counter")))
                                    NilIts)))
            NilIts)))

treeT5 = OpenFuncao (DefFuncao (Name "count25") NilIts
            (ConsIts (NestedLet (Let (ConsIts (Decl "b" (Const 100)) NilIts)
                                ((Inc (Var "a")))))
            NilIts))

treeT6 = OpenFuncao (DefFuncao (Name "count25") (ConsIts (Arg (Var "counter2")) NilIts)
            ((ConsIts (NestedFuncao (Funcao (Name "count25") (ConsIts (Arg (Var "counter2")) NilIts)))
            NilIts)))

treeT7 = OpenFuncao (DefFuncao (Name "count25") (ConsIts (Arg (Var "counter")) NilIts)
            (ConsIts (Increment (Var "counter"))
            (ConsIts (NestedIf (If (Less (Var "counter") (Const 5))
                                    (ConsIts (NestedFuncao (Funcao (Name "count25") (ConsIts (Arg (Var "counter")) NilIts)))
                                    NilIts)))
            NilIts)))


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
                 | CWhile
                 | CConsIts
                 | CNilIts
                 deriving Show

constructor :: (Typeable a) => Zipper a -> Constructor
constructor a = case (getHole a :: Maybe Exp) of
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
                        otherwise                 -> case (getHole a :: Maybe While) of
                            Just (While _ _)         -> CWhile
                            otherwise                 -> case (getHole a :: Maybe Items) of
                                Just (ConsIts _ _)    -> CConsIts
                                Just (NilIts)         -> CNilIts
                                otherwise                -> error "Error in constructor"


lev :: (Typeable a) => Zipper a -> Int
lev ag = case (constructor ag) of
            CLet -> 1 + lev (parent ag)
            CFuncao -> 1 + lev (parent ag)
            CDefFuncao -> 1 + lev (parent ag)
            CIf -> 1 + lev (parent ag)
            CWhile -> 1 + lev (parent ag)
            COpenFuncao -> 0
            COpenIf -> 0
            COpenLet -> 0
            COpenWhile -> 0
            CNilIts -> 1 + lev (parent ag)
            otherwise -> lev (parent ag)

dcli :: Typeable a => Zipper a -> Env
dcli t = case constructor t of
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
            CInc -> case  (constructor $ parent t) of -- (ignore) fazer isto pra todas as exp
                    CLet -> dclo (parent t.$1) ++ dcli (parent t)
                    _ -> dcli (parent t)
            CDec -> dcli (parent t)
            CReturn -> dcli (parent t)
            CBool -> dcli (parent t)
            CDecl -> dcli (parent t)
            CArg -> dcli (parent t)
            CIncrement -> dcli (parent t)
            CDecrement -> dcli (parent t)
            CConsIts ->  case (constructor $ parent t) of
                            CDefFuncao -> case arity t of
                                2 -> [(lexeme $ parent t, lev $ parent t)]
                                3 -> [(lexeme $ parent t, lev $ parent t)] ++ dclo ((parent t.$2))
                            _ -> dcli (parent t)
            CNilIts -> dcli (parent t)
            CNestedIf -> case  (constructor $ parent t) of
                            CLet    -> env (parent t)
                            CDefFuncao    -> env (parent t)
                            CIf -> env (parent t)
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
                            otherwise -> dcli (parent t)
            CNestedWhile -> case  (constructor $ parent t) of
                            CLet    -> env (parent t)
                            CDefFuncao    -> env (parent t)
                            CIf -> env (parent t)
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
                            otherwise -> dcli (parent t)
            CNestedLet -> case  (constructor $ parent t) of
                            CLet    -> env (parent t)
                            CDefFuncao    -> env (parent t)
                            CIf -> env (parent t)
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
                            otherwise -> dcli (parent t)
            CNestedFuncao -> dcli (parent t)
            CNestedReturn -> case  (constructor $ parent t) of
                            CLet    -> env (parent t)
                            CDefFuncao    -> env (parent t)
                            CIf -> env (parent t)
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
                            otherwise -> dcli (parent t)
            CLet -> case  (constructor $ parent t) of
                            CLet    -> env (parent t)
                            CDefFuncao    -> env (parent t)
                            CIf -> env (parent t)
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
                            otherwise -> dcli (parent t)
            CFuncao -> dcli (parent t)
            CDefFuncao -> case  (constructor $ parent t) of
                            COpenFuncao -> []
                            COpenIf -> []
                            COpenLet -> []
                            COpenWhile -> []
                            otherwise -> dcli (parent t)
            CIf -> case  (constructor $ parent t) of
                            CNestedIf -> env (parent t)
                            COpenIf -> []
            CWhile -> case  (constructor $ parent t) of
                            CNestedWhile -> env (parent t)
                            COpenWhile -> []
            CName -> dcli (parent t)
            COpenFuncao -> []
            COpenIf -> []
            COpenLet -> []
            COpenWhile -> []


lexeme a = case (getHole a :: Maybe Exp) of
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
dclo t =  case constructor t of
            CDecl -> (lexeme t,lev t) : (dcli t)
            CVar -> (lexeme t,lev t) : (dcli t)
            CFuncao -> (lexeme t,lev t) : (dcli t)
            CDefFuncao -> [(lexeme t,lev t)] ++ (dcli t)
            COpenFuncao -> dclo (t.$1)
            CName -> (lexeme t,lev t) : (dcli t)
            CConsIts -> dclo (t.$1) ++ (dclo (t.$2))
            CArg -> case (constructor $ parent $ parent t) of 
                CFuncao -> dcli t
                CDefFuncao -> dclo (t.$1)
            otherwise -> dcli t

env :: Typeable a => Zipper a -> Env
env t =   case constructor t of
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
            CVar -> allDecls (parent t)
            CInc -> env (parent t)
            CDec -> env (parent t)
            CReturn -> env (parent t)
            CBool -> env (parent t)
            CDecl -> env (parent t)
            CArg -> env (parent t)
            CIncrement -> env (parent t)
            CDecrement -> env (parent t)
            COpenFuncao -> []
            COpenIf -> []
            COpenLet -> []
            COpenWhile -> []
            CConsIts -> env (parent t)
            CNilIts -> env (parent t)
            _ -> case (constructor $ parent t) of
                    CLet    -> env (parent t)
                    CDefFuncao    -> env (parent t)
                    CIf -> env (parent t)
                    CWhile -> env (parent t)
                    CNestedWhile -> env (parent t)
                    CNestedFuncao -> env (parent t)
                    CNestedLet -> env (parent t)
                    CNestedReturn -> env (parent t)
                    COpenFuncao -> env (parent t)
                    COpenIf -> env (parent t)
                    COpenLet -> env (parent t)
                    COpenWhile -> env (parent t)
                    otherwise -> dcli t ++ env (parent t)

allDecls :: Typeable a => Zipper a -> Env
allDecls ag = case constructor ag of
    COpenFuncao -> []
    COpenIf -> []
    COpenLet -> []
    COpenWhile -> []
    _ -> dcli ag ++ env ag

mustBeIn :: String -> Env -> Errors
mustBeIn name [] = [name]
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
    CNot -> (scopes (ag.$1))
    CConst -> []
    CVar -> mustBeIn (lexeme ag) (env ag ++ dcli ag)
    CInc -> (scopes (ag.$1))
    CDec -> (scopes (ag.$1))
    CReturn -> (scopes (ag.$1))
    CBool -> []
    CDecl -> mustNotBeIn (lexeme (ag.$1), lev ag) (dcli ag) ++ (scopes (ag.$2))
    CArg -> case (constructor $ parent $ parent ag) of 
                CDefFuncao -> []
                _ -> (scopes (ag.$1))
    CIncrement -> scopes (ag.$1)
    CDecrement -> scopes (ag.$1)
    CNestedIf -> scopes (ag.$1)
    CNestedWhile -> scopes (ag.$1)
    CNestedLet -> scopes (ag.$1)
    CNestedFuncao -> scopes (ag.$1)
    CNestedReturn -> scopes (ag.$1)
    CLet -> (scopes (ag.$1)) ++ (scopes (ag.$2))
    CFuncao -> mustBeIn (lexeme (ag.$1)) (env ag ++ dcli ag) ++ (scopes (ag.$2))
    CDefFuncao -> mustNotBeIn (lexeme (ag.$1), lev ag) (dcli ag) ++ (scopes (ag.$2)) ++ (scopes (ag.$3))
    CName -> []
    CIf -> (scopes (ag.$1)) ++ (scopes (ag.$2))
    CWhile -> (scopes (ag.$1)) ++ (scopes (ag.$2)) 
    COpenFuncao -> scopes (ag.$1)
    COpenIf -> scopes (ag.$1)
    COpenLet -> scopes (ag.$1)
    COpenWhile -> scopes (ag.$1)
    CConsIts -> (scopes (ag.$1)) ++ (scopes (ag.$2))
    CNilIts -> []

