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

instance StrategicData (C.Item) where
  isTerminal t = isJust (getHole t :: Maybe Int)
              || isJust (getHole t :: Maybe String)
              || isJust (getHole t :: Maybe Bool)

--Test block translator
main'''' a = I.build $ mkAG a
--Test block processor for toy_C
main'' a = block $ I.build $ mkAG a
--Test applyDirections
dir' a b = S.lexeme $ I.applyDirections (mkAG a) b

----------------------------------------------------------------------------------------------------------------------------------------------
-- Imperative oriented block processor
----------------------------------------------------------------------------------------------------------------------------------------------
dclo :: BS.AGTree BS.Env
dclo t =  case BS.constructor t of
                    BS.CNilIts   -> dcli t
                    BS.CConsIts  -> dclo (t.$2)
                    BS.CDecl     -> (BS.lexeme t,lev t, (fromJust $ getHole t)) : (dcli t)
                    BS.CUse      -> dcli t
                    BS.CBlock    -> dcli t

errors :: BS.AGTree BS.Errors
errors t =  case BS.constructor t of
                       BS.CRoot     -> errors (t.$1)    
                       BS.CNilIts   -> []
                       BS.CConsIts  -> (errors (t.$1)) ++ (errors (t.$2))
                       BS.CBlock    -> errors (t.$1)                    
                       BS.CUse      -> BS.mustBeIn (BS.lexeme t) (fromJust $ getHole t) (dcli t)
                       BS.CDecl     -> BS.mustNotBeIn (BS.lexeme t,lev t) (fromJust $ getHole t) (dcli t)


---- Inheritted Attributes ----

dcli :: BS.AGTree BS.Env 
dcli t =  case BS.constructor t of
                    BS.CRoot     -> []
                    BS.CNilIts   -> case  (BS.constructor $ parent t) of
                                             BS.CConsIts  -> dclo (t.$<1)
                                             BS.CBlock    -> env (parent t)
                                             BS.CRoot     -> []
                    BS.CConsIts  -> case  (BS.constructor $ parent t) of
                                             BS.CConsIts  -> dclo (t.$<1)
                                             BS.CBlock    -> env (parent t)
                                             BS.CRoot     -> []
                    BS.CBlock    -> dcli (parent t)
                    BS.CUse      -> dcli (parent t)
                    BS.CDecl     -> dcli (parent t)
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
env :: BS.AGTree BS.Env
env t =  case BS.constructor t of
                    BS.CRoot      ->  dclo t
                    BS.CBlock     ->  env (parent t)
                    BS.CUse       ->  env (parent t)
                    BS.CDecl      ->  env (parent t)
                    _          ->  case (BS.constructor $ parent t) of
                                                BS.CBlock    -> dclo t
                                                BS.CConsIts  -> env (parent t)
                                                BS.CRoot     -> dclo t


block :: BS.P -> BS.Errors
block p = errors (mkAG p)