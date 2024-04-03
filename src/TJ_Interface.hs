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
                        TJ.CDefClass -> B.ConsIts (B.Decl (TJ.lexeme a) d) (I.buildChildren build' a d)
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
                                TJ.CGlobal -> B.ConsIts (B.Decl (TJ.lexeme a) d) (I.buildChildren globals a d)
                                TJ.CDefClass -> B.ConsIts (B.Decl (TJ.lexeme a) d) (I.buildChildren globals a d)
            | otherwise = (I.buildChildren globals a d)


main a = build $ mkAG a
main' a = globals' $ mkAG a
main'' a = block $ build $ mkAG a
main''' a = env $ mkAG $ globals' $ mkAG a

--type Env    = [(Name, Int, It)]
--type Errors = [(Name, It, String)]

dclo :: BS.AGTree B.Env
dclo t =  case BS.constructor t of
                    BS.CNilIts   -> dcli t
                    BS.CConsIts  -> dclo (t.$2)
                    BS.CDecl     -> (BS.lexeme t,lev t, (fromJust $ getHole t)) : (dcli t)
                    BS.CUse      -> dcli t
                    BS.CBlock    -> dcli t

errors :: BS.AGTree B.Errors
errors t =  case BS.constructor t of
                       BS.CRoot     -> errors (t.$1)    
                       BS.CNilIts   -> []
                       BS.CConsIts  -> (errors (t.$1)) ++ (errors (t.$2))
                       BS.CBlock    -> errors (t.$1)                    
                       BS.CUse      -> BS.mustBeIn (BS.lexeme t) (fromJust $ getHole t) (env t)
                       BS.CDecl     -> BS.mustNotBeIn (BS.lexeme t,lev t) (fromJust $ getHole t) (dcli t)


---- Inheritted Attributes ----

dcli :: BS.AGTree B.Env 
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
env :: BS.AGTree B.Env
env t =  case BS.constructor t of
                    BS.CRoot      ->  trace "1" $ dclo t
                    BS.CBlock     ->  trace "2" $ env (parent t)
                    BS.CUse       ->  trace "3" $ env (parent t)
                    BS.CDecl      ->  trace "4" $ env (parent t)
                    _          ->  case (BS.constructor $ parent t) of
                                                BS.CBlock    -> trace "5" $ dclo t
                                                BS.CConsIts  -> trace "6" $ env (parent t)
                                                BS.CRoot     -> trace "7" $ dclo t


block :: B.P -> B.Errors
block p = errors (mkAG p)
