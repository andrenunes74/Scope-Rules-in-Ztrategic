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

children :: Scopes a => (Zipper a -> B.Directions -> B.Its) -> Zipper a -> B.Directions -> [B.Its]
children buildFunc ag d = case S.down' ag of 
  Nothing -> []
  Just n -> do  
    let x = d ++ [B.D] 
    buildFunc n x : brothers buildFunc n x  

brothers :: Scopes a => (Zipper a -> B.Directions -> B.Its) -> Zipper a -> B.Directions -> [B.Its]
brothers buildFunc ag d = case S.right ag of 
  Nothing -> []
  Just n -> do 
    let x = d ++ [B.R] 
    buildFunc n x : brothers buildFunc n x  


buildChildren :: Scopes a => (Zipper a -> B.Directions -> B.Its) -> Zipper a -> B.Directions -> B.Its
buildChildren buildFunc ag d = foldr mergeIts B.NilIts (children buildFunc ag d) 

applyDirections :: Scopes a => Zipper a -> B.Directions -> Zipper a
applyDirections ag [] = ag
applyDirections ag (h:t)  | h == B.D = case S.down' ag of 
                                        Nothing -> error "can't go there"
                                        Just n -> applyDirections n t 
                          | h == B.R = case S.right ag of 
                                        Nothing -> error "can't go there"
                                        Just n -> applyDirections n t 
                          | h == B.U = case S.up ag of 
                                        Nothing -> error "can't go there"
                                        Just n -> applyDirections n t 
                          | h == B.L = case S.left ag of 
                                        Nothing -> error "can't go there"
                                        Just n -> applyDirections n t 