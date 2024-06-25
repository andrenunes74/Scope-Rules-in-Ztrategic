{-# LANGUAGE DeriveDataTypeable #-}

module Core_Interface where
import qualified Scopes as S
import qualified Block.Shared as B
import qualified Block.SharedAG as BS
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
    isGlobal ag = False
    getDecl a = S.lexeme a
    getUse a = S.lexeme a
    initialState ag = ["e"]

instance StrategicData (C.Item) where
  isTerminal t = isJust (getHole t :: Maybe Int)
              || isJust (getHole t :: Maybe String)
              || isJust (getHole t :: Maybe Bool)

--Test block translator
toBlock a = I.build $ mkAG a
--Test block processor for Core
doAllThings a = block (I.string2Env (I.initialState a)) (I.build $ mkAG a)
--Test applyDirections
dir' a b = S.lexeme $ I.applyDirections (mkAG a) b

----------------------------------------------------------------------------------------------------------------------------------------------
-- Imperative oriented block processor
----------------------------------------------------------------------------------------------------------------------------------------------
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
                       BS.CUse      -> mustBeIn (BS.lexeme t) (fromJust $ getHole t) (dcli a t) a
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

block :: BS.Env -> BS.P -> B.Errors
block a p = errors a (mkAG p)