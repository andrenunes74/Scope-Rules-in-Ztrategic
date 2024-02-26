{-# LANGUAGE DeriveDataTypeable #-}

module Interface where
import qualified Scopes as S
import qualified Block.Shared as B
import qualified Core as C
import qualified ToBlock as TB
import qualified Block.Block_Zippers as M
import Data.Data
import Data.Generics.Zipper
import Library.StrategicData (StrategicData)
import Library.Ztrategic
import Data.Generics.Aliases
import Library.ZipperAG
import Data.Maybe
import Debug.Trace

class Typeable a => Scopes a where
    isDecl :: Typeable a => Zipper a -> Bool
    isUse :: Typeable a => Zipper a -> Bool
    isBlock :: Typeable a => Zipper a -> Bool

instance Scopes (C.Item) where
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
    
build :: Scopes a => Zipper a -> B.P
build a = B.Root (build' $ a.$1)

build' :: Scopes a => Zipper a -> B.Its
build' a | isDecl a = case (S.constructor a) of
                        S.CDecl -> B.ConsIts (B.Decl (S.lexeme a)) (build' $ a.$2)
                        S.CDefFuncao -> B.ConsIts (B.Decl (S.lexeme a)) (TB.mergeIts (build' $ a.$2) (build' $ a.$3))
                        S.CVar -> B.ConsIts (B.Decl (S.lexeme a)) B.NilIts
         | isUse a = case (S.constructor a) of
                       S.CVar -> B.ConsIts (B.Use (S.lexeme a)) B.NilIts
                       S.CFuncao -> B.ConsIts (B.Use (S.lexeme a)) (build' $ a.$2)
         | isBlock a = B.ConsIts (B.Block $ buildChildren a) B.NilIts
         | otherwise = case (S.constructor a) of
                            S.CConst -> B.NilIts
                            S.CBool -> B.NilIts
                            S.CNilIts -> B.NilIts
                            _ -> buildChildren a

children :: Scopes a => Zipper a -> [B.Its]
children ag = case down' ag of 
  Nothing -> []
  Just n -> build' n : brothers n 

brothers :: Scopes a => Zipper a -> [B.Its]
brothers ag = case right ag of 
 Nothing -> []
 Just n -> build' n : brothers n 

buildChildren :: Scopes a => Zipper a -> B.Its
buildChildren ag = foldr TB.mergeIts B.NilIts (children ag) 

main'' a = M.block $ build $ mkAG a