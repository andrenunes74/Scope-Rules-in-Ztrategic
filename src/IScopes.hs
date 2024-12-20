{-# LANGUAGE DeriveDataTypeable #-}

module IScopes (Scopes(..), applyErrors_a68, applyErrors_io, processor_a68, processor_io ,string2Env, buildChildren, applyErrors, applyDirections, build, toBlock, B.Errors, B.P, S.StrategicData(..)) where
import Data.Data ( Data, Typeable )
import Data.Generics.Zipper
import Library.Ztrategic
import Data.Generics.Aliases
import Library.ZipperAG
import Data.Maybe
import Debug.Trace
import qualified Library.StrategicData as S (StrategicData(..), isJust, left, right, up, down')
import qualified Block.Shared as B
import qualified Block.Block_Zippers as BZ
import qualified Block.SharedAG as BS
import qualified BlockA68 as BA
import qualified BlockC as BC

class (Typeable a, S.StrategicData a, Data a) => Scopes a where
    isDecl :: Zipper a -> Bool
    isUse :: Zipper a -> Bool
    isBlock :: Zipper a -> Bool
    isGlobal :: Zipper a -> Bool
    isGlobal _ = False
    getUse :: Zipper a -> String
    getUse a = getString a
    getDecl :: Zipper a -> [String]
    getDecl a = [getString a]
    setUse :: Zipper a -> String -> Zipper a
    setUse a b = modifyFunc a b
    setDecl :: Zipper a -> String-> Zipper a
    setDecl a b = modifyFunc a b
    initialState :: a -> [String]
    initialState = const []

toBlock :: Scopes a => a -> B.P
toBlock = build . toZipper

build :: Scopes a => Zipper a -> B.P
build a = B.Root (buildChildren build' a [])

build' :: Scopes a => Zipper a -> B.Directions -> B.Its
-- build' a d | isDecl a = B.ConsIts (B.Decl (getDecl a) d) (buildChildren build' a d)
build' a d | isDecl a = mergeIts (foldr (B.ConsIts . (`B.Decl` d)) B.NilIts (getDecl a)) (buildChildren build' a d)
           | isUse a = B.ConsIts (B.Use (getUse a) d) (buildChildren build' a d)
           | isBlock a = B.ConsIts (B.Block $ buildChildren build' a d) B.NilIts
           | otherwise = buildChildren build' a d

mergeIts :: B.Its -> B.Its -> B.Its
mergeIts (B.NilIts) its = its
-- clean empty blocks
mergeIts (B.ConsIts (B.Block B.NilIts) xs) its = mergeIts xs its
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

modifyZipperAlongPath :: Scopes a => Zipper a -> (BS.Name, BS.It, String) -> Zipper a
modifyZipperAlongPath ag (_, B.Use   _ d, e)  = setUse  (applyDirections ag d) e
modifyZipperAlongPath ag (_, B.Decl  _ d, e)  = setDecl (applyDirections ag d) e

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

getString :: Scopes a => Zipper a -> String
getString ag = case down' ag of
  Nothing -> error "Error going down on getString"
  Just n -> case (getHole n :: Maybe String) of
                Just holeString -> holeString
                Nothing -> getString' n

getString' :: Scopes a => Zipper a -> String
getString' ag = case right ag of
  Nothing -> error "Error going right on getString"
  Just n -> case (getHole n :: Maybe String) of
                Just holeString -> holeString
                Nothing -> getString' n

applyErrors :: Scopes a => Zipper a -> B.Errors -> Zipper a
applyErrors ag [] = ag
applyErrors ag (e: t) = applyErrors (mkAG $ fromZipper $ modifyZipperAlongPath ag e) t

string2Env :: [String] -> B.Env
string2Env [] = []
string2Env (h:t) = (h,0,B.Decl h []) : string2Env t

----------------------------------------------------------------------------------------------------------------------------------------------
-- Processors
----------------------------------------------------------------------------------------------------------------------------------------------
processor_a68 a = BA.block_a68 (string2Env (initialState a)) (build $ mkAG a)

processor_io a = BC.block_io (string2Env (initialState a)) (build $ mkAG a)

applyErrors_a68 a = fromZipper $ applyErrors (toZipper a) (processor_a68 a)

applyErrors_io a = fromZipper $ applyErrors (toZipper a) (processor_io a)