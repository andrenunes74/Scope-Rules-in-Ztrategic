{-# LANGUAGE DeriveDataTypeable #-}

module TJ_Interface where
import qualified Block.Shared as B
import qualified Block.SharedAG as BS
import qualified IScopes as I 
import qualified Toy_Java as TJ
--import qualified Block.Block_Zippers as M
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

instance StrategicData (TJ.Items) where
  isTerminal t = isJust (getHole t :: Maybe Int)
              || isJust (getHole t :: Maybe String)
              || isJust (getHole t :: Maybe Bool)

build :: I.Scopes a => Zipper a -> B.P
build a = B.Root (I.buildChildren build' a [])

build' :: I.Scopes a => Zipper a -> B.Directions -> B.Its
build' a d | I.isDecl a = case (TJ.constructor a) of
                        TJ.CDecl -> B.ConsIts (B.Decl (TJ.lexeme a) d) (I.buildChildren build' a d)
                        TJ.CDefFuncao -> B.ConsIts (B.Decl (TJ.lexeme a) d) (I.buildChildren build' a d)
                        TJ.CVar -> B.ConsIts (B.Decl (TJ.lexeme a) d) B.NilIts
                        TJ.CDefClass -> B.ConsIts (B.Block (B.ConsIts (B.Decl (TJ.lexeme a) d) (I.buildChildren build' a d))) B.NilIts
           | I.isUse a = case (TJ.constructor a) of
                       TJ.CVar -> B.ConsIts (B.Use (TJ.lexeme a) d) B.NilIts
                       TJ.CFuncao -> B.ConsIts (B.Use (TJ.lexeme a) d) (I.buildChildren build' a d)
                       TJ.CClass -> B.ConsIts (B.Use (TJ.lexeme a) d) B.NilIts
           | I.isBlock a = B.ConsIts (B.Block (I.buildChildren build' a d)) B.NilIts
           | otherwise = (I.buildChildren build' a d)

globals' :: I.Scopes a => Zipper a -> B.P
globals' a = B.Root (I.buildChildren globals a [])

globals :: I.Scopes a => Zipper a -> B.Directions -> B.Its
globals a d | I.isGlobal a = case (TJ.constructor a) of
                                TJ.CGlobal -> B.ConsIts (B.Decl (TJ.lexeme a) (d++[B.D])) (I.buildChildren globals a d)
                                TJ.CDefClass -> B.ConsIts (B.Decl (TJ.lexeme a) d) (I.buildChildren globals a d)
            | otherwise = (I.buildChildren globals a d)


main a = build $ mkAG a
main' a = globals' $ mkAG a
main'' a = block (env [] (mkAG $ globals' $ mkAG a)) (build $ mkAG a)
main''' a = (env [] (mkAG $ globals' $ mkAG a))

--type Env    = [(Name, Int, It)]
--type Errors = [(Name, It, String)]

dclo :: BS.Env -> Zipper BS.P -> BS.Env
dclo a t =  case BS.constructor t of
                    BS.CNilIts   -> dcli a t
                    BS.CConsIts  -> dclo a (t.$2)
                    BS.CDecl     -> (BS.lexeme t,lev t, (fromJust $ getHole t)) : (dcli a t)
                    BS.CUse      -> dcli a t
                    BS.CBlock    -> dcli a t
                    BS.CRoot     -> dclo a (t.$1)

errors :: BS.Env -> Zipper BS.P -> B.Errors
errors a t =  case BS.constructor t of
                       BS.CRoot     -> errors a (t.$1)    
                       BS.CNilIts   -> []
                       BS.CConsIts  -> (errors a (t.$1)) ++ (errors a (t.$2))
                       BS.CBlock    -> errors a (t.$1)                    
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
    let names = map (\(name', _, _) -> name') e in
    if (name `elem` names) then
        [(name, item, " <= [Duplicated declaration!]")]
    else
        []

mustNotBeInA :: BS.Name -> BS.It -> BS.Env -> BS.Errors
mustNotBeInA name item a =
    let items = filter (\(name', _, item') -> name' == name && item /= item') a
        errorMsg = " <= [Different declaration!]"
    in
    if not (null items) then
       [(name, item, errorMsg)]
    else
        []

mustNotBeIn :: BS.Name -> BS.It -> BS.Env -> BS.Env -> BS.Errors
mustNotBeIn name item e a =
    mustNotBeInE name item e ++ mustNotBeInA name item a

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

block :: BS.Env -> BS.P -> B.Errors
block a p = errors a (mkAG p)
