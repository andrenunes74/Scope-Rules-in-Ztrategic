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

class (Typeable a, S.StrategicData a, Data a) => Scopes a where
    isDecl :: Zipper a -> Bool
    isUse :: Zipper a -> Bool
    isBlock :: Zipper a -> Bool
    isGlobal :: Zipper a -> Bool

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
                                        Nothing -> error "Can't go there!"
                                        Just n -> applyDirections n t 
                          | h == B.R = case S.right ag of 
                                        Nothing -> error "Can't go there!"
                                        Just n -> applyDirections n t 
                          | h == B.L = case S.left ag of 
                                        Nothing -> error "Can't go there!"
                                        Just n -> applyDirections n t 
                          | h == B.U = case S.up ag of 
                                        Nothing -> error "Can't go there!"
                                        Just n -> applyDirections n t 

modifyZipperAlongPath :: Scopes a => Zipper a -> B.Directions -> String -> Zipper a
modifyZipperAlongPath ag d e = modifyFunc (applyDirections ag d) e

modifyFunc :: Scopes a => Zipper a -> String -> Zipper a
modifyFunc ag s = case down' ag of 
  Nothing -> error "Error going down on modifyFunc"
  Just n -> case (getHole n :: Maybe String) of
                Just holeString -> setHole (holeString ++ s) n
                Nothing -> aux n s

aux :: Scopes a => Zipper a -> String -> Zipper a
aux ag s = case right ag of 
  Nothing -> error "Error going right on modifyFunc"
  Just n -> case (getHole n :: Maybe String) of
                Just holeString -> setHole (holeString ++ s) n
                Nothing -> aux n s

dirs :: B.Errors -> [(B.Directions,String)] 
dirs [] = []
dirs ((_,(B.Use a b), s):t) = (b,s) : dirs t
dirs ((_,(B.Decl a b), s):t) = (b,s) : dirs t

applyErrors :: Scopes a => Zipper a -> B.Errors -> Zipper a
applyErrors ag [] = ag
applyErrors ag e = do
  let er = dirs e
  applyErrors' ag er

applyErrors' :: Scopes a => Zipper a -> [(B.Directions,String)] -> Zipper a
applyErrors' ag [] = ag 
applyErrors' ag ((a,b): t) = applyErrors' (mkAG $ fromZipper $ (modifyZipperAlongPath ag a b)) t