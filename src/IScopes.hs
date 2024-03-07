{-# LANGUAGE DeriveDataTypeable #-}

module IScopes where
import Data.Data
import Data.Generics.Zipper
import Library.Ztrategic
import Data.Generics.Aliases
import Library.ZipperAG
import Data.Maybe
import Debug.Trace
import qualified Library.StrategicData as S (StrategicData(..), isJust, left, right, up, down') 
import qualified Block.Shared as B

class (Typeable a, S.StrategicData a) => Scopes a where
    isDecl :: (Typeable a, S.StrategicData a) => Zipper a -> Bool
    isUse :: (Typeable a, S.StrategicData a) => Zipper a -> Bool
    isBlock :: (Typeable a, S.StrategicData a) => Zipper a -> Bool

mergeIts :: B.Its -> B.Its -> B.Its
mergeIts (B.NilIts) its = its
mergeIts (B.ConsIts x xs) its = B.ConsIts x (mergeIts xs its)

children :: Scopes a => (Zipper a -> B.Its) -> Zipper a -> [B.Its]
children buildFunc ag = case S.down' ag of 
  Nothing -> []
  Just n -> buildFunc n : brothers buildFunc n

brothers :: Scopes a => (Zipper a -> B.Its) -> Zipper a -> [B.Its]
brothers buildFunc ag = case S.right ag of 
  Nothing -> []
  Just n -> buildFunc n : brothers buildFunc n

buildChildren :: Scopes a => (Zipper a -> B.Its) -> Zipper a -> B.Its
buildChildren buildFunc ag = foldr mergeIts B.NilIts (children buildFunc ag) 
