{-# LANGUAGE DeriveDataTypeable #-}

module IScopes (Scopes(..),env, block_a68, processor_a68, processor_io ,string2Env, buildChildren, applyErrors, applyDirections, build, toBlock, B.Errors, B.P, S.StrategicData(..)) where
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

class (Typeable a, S.StrategicData a, Data a) => Scopes a where
    isDecl :: Zipper a -> Bool
    isUse :: Zipper a -> Bool
    isBlock :: Zipper a -> Bool
    isGlobal :: Zipper a -> Bool
    isGlobal _ = False 
    getUse :: Zipper a -> String
    getUse a = getString a
    getDecl :: Zipper a -> String
    getDecl a = getString a
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
build' a d | isDecl a = B.ConsIts (B.Decl (getDecl a) d) (buildChildren build' a d)
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

string2Env :: [String] -> B.Env
string2Env [] = []
string2Env (h:t) = (h,0,B.Decl h []) : string2Env t

----------------------------------------------------------------------------------------------------------------------------------------------
-- Processors
----------------------------------------------------------------------------------------------------------------------------------------------
dclo :: BS.Env -> Zipper BS.P -> BS.Env
dclo a t = case BS.constructor t of
                    BS.CNilIts   -> dcli a t
                    BS.CConsIts  -> dclo a (t.$2)
                    BS.CDecl     -> (BS.lexeme t,lev t, (fromJust $ getHole t)) : (dcli a t)
                    BS.CUse      -> dcli a t
                    BS.CBlock    -> dcli a t
                    BS.CRoot     -> dclo a (t.$1)

errors_a68 :: BS.Env -> Zipper BS.P -> B.Errors
errors_a68 a t =  case BS.constructor t of
                       BS.CRoot     -> errors_a68 a (t.$1)    
                       BS.CNilIts   -> []
                       BS.CConsIts  -> (errors_a68 a (t.$1)) ++ (errors_a68 a (t.$2))
                       BS.CBlock    -> errors_a68 a (t.$1)                    
                       BS.CUse      -> mustBeIn (BS.lexeme t) (fromJust $ getHole t) (env a t) a
                       BS.CDecl     -> mustNotBeIn (BS.lexeme t) (fromJust $ getHole t) (dcli a t) a

dcli :: BS.Env -> Zipper BS.P -> BS.Env
dcli a t =  case BS.constructor t of
                    BS.CRoot     -> a
                    BS.CNilIts   -> case  (BS.constructor $ parent t) of
                                             BS.CConsIts  -> dclo a (t.$<1)
                                             BS.CBlock    -> env a (parent t)
                                             BS.CRoot     -> []
                    BS.CConsIts  -> case  (BS.constructor $ parent t) of
                                             BS.CConsIts  -> dclo a (t.$<1)
                                             BS.CBlock    -> env a (parent t)
                                             BS.CRoot     -> []
                    BS.CBlock    -> dcli a (parent t)
                    BS.CUse      -> dcli a (parent t)
                    BS.CDecl     -> dcli a (parent t)

lev :: BS.AGTree Int
lev t =  case BS.constructor t of
                     BS.CRoot     ->  0
                     BS.CBlock    ->  lev (parent t)
                     BS.CUse      ->  lev (parent t)
                     BS.CDecl     ->  lev (parent t)
                     _         ->  case  (BS.constructor $ parent t) of
                                        BS.CBlock    -> (lev (parent t)) + 1
                                        BS.CConsIts  -> lev (parent t)
                                        BS.CRoot     -> 0

env :: BS.Env -> Zipper BS.P -> BS.Env
env a t =  case BS.constructor t of
                    BS.CRoot      ->  dclo a t
                    BS.CBlock     ->  env a (parent t)
                    BS.CUse       ->  env a (parent t)
                    BS.CDecl      ->  env a (parent t)
                    _          ->  case (BS.constructor $ parent t) of
                                                BS.CBlock    -> dclo a t
                                                BS.CConsIts  -> env a (parent t)
                                                BS.CRoot     -> dclo a t


mustBeIn :: BS.Name -> BS.It -> BS.Env -> BS.Env -> BS.Errors
mustBeIn n i e a = if (null (filter ((== n) . fst3) (e++a))) 
                   then [(n, i, " <= [Undeclared use!]")] else []

mustNotBeInE :: BS.Name ->  BS.It -> BS.Env -> BS.Errors
mustNotBeInE name item e =
    let names = map (\(name', _, _) -> name') e 
        errorMsg = " <= [Duplicated declaration!]"
    in
    if (name `elem` names) then [(name, item, errorMsg)] else []

mustNotBeInA :: BS.Name -> BS.It -> BS.Env -> BS.Errors
mustNotBeInA name item a =
    let items = filter (\(name', _, item') -> name' == name && item /= item') a
        errorMsg = " <= [Different declaration!]"
    in
    if not (null items) then [(name, item, errorMsg)] else []

mustNotBeIn :: BS.Name -> BS.It -> BS.Env -> BS.Env -> BS.Errors
mustNotBeIn name item e a =
    mustNotBeInE name item e ++ mustNotBeInA name item a

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

block_a68 :: BS.Env -> BS.P -> B.Errors
block_a68 a p = errors_a68 (a) (mkAG p)

errors_io :: BS.Env -> Zipper BS.P -> B.Errors
errors_io a t =  case BS.constructor t of
                       BS.CRoot     -> errors_io a (t.$1)    
                       BS.CNilIts   -> []
                       BS.CConsIts  -> (errors_io a (t.$1)) ++ (errors_io a (t.$2))
                       BS.CBlock    -> errors_io a (t.$1)                    
                       BS.CUse      -> mustBeIn (BS.lexeme t) (fromJust $ getHole t) (dcli a t) a
                       BS.CDecl     -> mustNotBeIn (BS.lexeme t) (fromJust $ getHole t) (dcli a t) a

block_io :: BS.Env -> BS.P -> B.Errors
block_io a p = errors_io a (mkAG p)

processor_a68 a = block_a68 (string2Env (initialState a)) (build $ mkAG a)

processor_io a = block_io (string2Env (initialState a)) (build $ mkAG a)