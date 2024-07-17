{-# LANGUAGE DeriveDataTypeable #-}

module TJ_Interface where
import qualified Block.Shared as B
import qualified Block.SharedAG as BS
import qualified IScopes as I 
import qualified Toy_Java as TJ
import Data.Data
import Data.Generics.Zipper
import Library.StrategicData (StrategicData(..), isJust)
import Library.Ztrategic
import Data.Generics.Aliases
import Library.ZipperAG
import Data.Maybe hiding (isJust)
import Debug.Trace

instance I.Scopes (TJ.Items) where
    isDecl ag = case (TJ.constructor ag) of
        TJ.CDecl -> True
        TJ.CDefFuncao -> True
        TJ.CVar -> case (TJ.constructor $ parent $ parent $ parent ag) of 
                    TJ.CDefFuncao -> case arity $ parent $ parent ag of
                                        2 -> True
                                        3 -> False
                    _ -> False
        TJ.CDefClass -> True
        _ -> False
    isUse ag = case (TJ.constructor ag) of
        TJ.CVar -> True
        TJ.CFuncao -> True
        TJ.CClass -> True
        _ -> False
    isBlock ag = case (TJ.constructor ag) of
        TJ.CIf -> True
        TJ.CWhile -> True
        TJ.CLet -> True
        _ -> False
    isGlobal ag = case (TJ.constructor ag) of
        TJ.CGlobal -> True
        TJ.CDefClass -> True
        _ -> False
    initialState ag = ["Class2"]

instance StrategicData (TJ.Items) where
  isTerminal t = isJust (getHole t :: Maybe Int)
              || isJust (getHole t :: Maybe String)
              || isJust (getHole t :: Maybe Bool)

build :: I.Scopes a => Zipper a -> B.P
build a = B.Root (I.buildChildren build' a [])

build' :: I.Scopes a => Zipper a -> B.Directions -> B.Its
build' a d | I.isDecl a = case (TJ.constructor a) of
                        TJ.CVar -> B.ConsIts (B.Decl (TJ.lexeme a) d) B.NilIts 
                        TJ.CDefClass -> B.ConsIts (B.Block (B.ConsIts (B.Decl (TJ.lexeme a) d) (I.buildChildren build' a d))) B.NilIts
                        _ -> B.ConsIts (B.Decl (TJ.lexeme a) d) (I.buildChildren build' a d)
           | I.isUse a = case (TJ.constructor a) of
                       TJ.CFuncao -> B.ConsIts (B.Use (TJ.lexeme a) d) (I.buildChildren build' a d)
                       _ -> B.ConsIts (B.Use (TJ.lexeme a) d) B.NilIts
           | I.isBlock a = B.ConsIts (B.Block (I.buildChildren build' a d)) B.NilIts
           | otherwise = (I.buildChildren build' a d)

globals' :: I.Scopes a => Zipper a -> B.P
globals' a = B.Root (I.buildChildren globals a [])

globals :: I.Scopes a => Zipper a -> B.Directions -> B.Its
globals a d | I.isGlobal a = case (TJ.constructor a) of
                                TJ.CGlobal -> B.ConsIts (B.Decl (TJ.lexeme a) (d++[B.D])) (I.buildChildren globals a d)
                                TJ.CDefClass -> B.ConsIts (B.Decl (TJ.lexeme a) d) (I.buildChildren globals a d)
            | otherwise = (I.buildChildren globals a d)

--Test block processor for toy_java
doAllThings a = I.block_a68 ((I.env [] (mkAG $ globals' $ mkAG a))++(I.string2Env (I.initialState a))) (build $ mkAG a)