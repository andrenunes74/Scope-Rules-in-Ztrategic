{-# LANGUAGE DeriveDataTypeable #-}

module Core_Interface where
import qualified Scopes as S
import qualified Block.Shared as B
import qualified Core as C
import qualified Block.Block_Zippers as M
import qualified IScopes as I 
import Data.Data
import Data.Generics.Zipper
import Library.StrategicData (StrategicData(..), isJust)
import Library.Ztrategic
import Data.Generics.Aliases
import Library.ZipperAG
import Data.Maybe hiding (isJust)
import Debug.Trace

instance I.Scopes (C.Item) where
    isDecl ag = case (S.constructor ag) of
        S.CDecl -> True
        S.CDefFuncao -> True
        S.CVar -> case (S.constructor $ parent $ parent $ parent ag) of 
                    S.CDefFuncao -> case arity $ parent $ parent ag of
                                        2 -> True
                                        3 -> False
                    _ -> False
        _ -> False
    isUse ag = case (S.constructor ag) of
        S.CVar -> True
        S.CFuncao -> True
        _ -> False
    isBlock ag = case (S.constructor ag) of
        S.CIf -> True
        S.CWhile -> True
        S.CLet -> True
        _ -> False

instance StrategicData (C.Item) where
  isTerminal t = isJust (getHole t :: Maybe Int)
              || isJust (getHole t :: Maybe String)
              || isJust (getHole t :: Maybe Bool)
        
build :: I.Scopes a => Zipper a -> B.P
build a = B.Root (I.buildChildren build' a)

build' :: I.Scopes a => Zipper a -> B.Its
build' a | I.isDecl a = case (S.constructor a) of
                        S.CDecl -> B.ConsIts (B.Decl (S.lexeme a)) (I.buildChildren build' a)
                        S.CDefFuncao -> B.ConsIts (B.Decl (S.lexeme a)) (I.buildChildren build' a)
                        S.CVar -> B.ConsIts (B.Decl (S.lexeme a)) B.NilIts
         | I.isUse a = case (S.constructor a) of
                       S.CVar -> B.ConsIts (B.Use (S.lexeme a)) B.NilIts
                       S.CFuncao -> B.ConsIts (B.Use (S.lexeme a)) (I.buildChildren build' a)
         | I.isBlock a = B.ConsIts (B.Block (I.buildChildren build' a)) B.NilIts
         | otherwise = (I.buildChildren build' a)

main'' a = M.block $ build $ mkAG a