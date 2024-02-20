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
    isRoot :: Typeable a => Zipper a -> Bool
    isIts :: Typeable a => Zipper a -> Bool
    isEndIts :: Typeable a => Zipper a -> Bool
    isExp :: Typeable a => Zipper a -> Bool

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
    isRoot ag = case (S.constructor ag) of
        S.COpenFuncao -> True
        S.COpenWhile -> True
        S.COpenLet -> True
        S.COpenIf -> True
        _ -> False
    isBlock ag = case (S.constructor ag) of
        S.CIf -> True
        S.CWhile -> True
        S.CLet -> True
        _ -> False
    isIts ag = case (S.constructor ag) of
        S.CConsIts -> True
        _ -> False
    isEndIts ag = case (S.constructor ag) of
        S.CNilIts -> True
        _ -> False
    isExp ag = case (S.constructor ag) of
        S.CAdd -> True
        S.CSub -> True
        S.CDiv -> True
        S.CMul -> True
        S.CLess -> True
        S.CGreater -> True
        S.CEquals -> True
        S.CGTE -> True
        S.CLTE -> True
        S.CAnd -> True
        S.COr -> True
        _ -> False
    

build :: Scopes a => Zipper a -> B.P
build a = if isRoot a then B.Root (build' $ a.$1) else B.Root B.NilIts

build' :: Scopes a => Zipper a -> B.Its
build' a | isDecl a = case (S.constructor a) of
                        S.CDecl -> B.ConsIts (B.Decl (S.lexeme a)) (build' $ a.$2)
                        S.CDefFuncao -> B.ConsIts (B.Decl (S.lexeme a)) (TB.mergeIts (build' $ a.$2) (build' $ a.$3))
                        S.CVar -> B.ConsIts (B.Decl (S.lexeme a)) B.NilIts
         | isUse a = case (S.constructor a) of
                       S.CVar -> B.ConsIts (B.Use (S.lexeme a)) B.NilIts
                       S.CFuncao -> B.ConsIts (B.Use (S.lexeme a)) (build' $ a.$2)
         | isBlock a = B.ConsIts (B.Block (TB.mergeIts (build' $ a.$1) (build' $ a.$2))) B.NilIts
         | isIts a = (TB.mergeIts (build' $ a.$1) (build' $ a.$2))
         | isEndIts a = B.NilIts
         | isExp a = (TB.mergeIts (build' $ a.$1) (build' $ a.$2))
         | otherwise = case (S.constructor a) of
                            S.CConst -> B.NilIts
                            S.CBool -> B.NilIts
                            _ -> (build' $ a.$1)

main'' a = M.block $ build $ mkAG a