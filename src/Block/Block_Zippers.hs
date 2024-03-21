{-# LANGUAGE DeriveDataTypeable #-}


-----------------------------------------------------------------------
--
--  The Fun of Programming With Attribute Grammars
--
--  code presented in Section 5
--
----------------------------------------------------------------------


module Block.Block_Zippers where

import Library.ZipperAG hiding (mkAG,(.$>),(.$<))
import Data.Generics.Zipper
import Data.Maybe
import Data.Data
import Block.SharedAG

-- AG

---- Synthesized Attributes ----

dclo :: AGTree Env
dclo t =  case constructor t of
                    CNilIts   -> dcli t
                    CConsIts  -> dclo (t.$2)
                    CDecl     -> (lexeme t,lev t, (fromJust $ getHole t)) : (dcli t)
                    CUse      -> dcli t
                    CBlock    -> dcli t

errors :: AGTree Errors
errors t =  case constructor t of
                       CRoot     -> errors (t.$1)    
                       CNilIts   -> []
                       CConsIts  -> (errors (t.$1)) ++ (errors (t.$2))
                       CBlock    -> errors (t.$1)                    
                       CUse      -> mustBeIn (lexeme t) (fromJust $ getHole t) (env t)
                       CDecl     -> mustNotBeIn (lexeme t,lev t) (fromJust $ getHole t) (dcli t)


---- Inheritted Attributes ----

dcli :: AGTree Env 
dcli t =  case constructor t of
                    CRoot     -> []
                    CNilIts   -> case  (constructor $ parent t) of
                                             CConsIts  -> dclo (t.$<1)
                                             CBlock    -> env (parent t)
                                             CRoot     -> []
                    CConsIts  -> case  (constructor $ parent t) of
                                             CConsIts  -> dclo (t.$<1)
                                             CBlock    -> env (parent t)
                                             CRoot     -> []
                    CBlock    -> dcli (parent t)
                    CUse      -> dcli (parent t)
                    CDecl     -> dcli (parent t)
lev :: AGTree Int
lev t =  case constructor t of
                     CRoot     ->  0
                     CBlock    ->  lev (parent t)
                     CUse      ->  lev (parent t)
                     CDecl     ->  lev (parent t)
                     _         ->  case  (constructor $ parent t) of
                                        CBlock    -> (lev (parent t)) + 1
                                        CConsIts  -> lev (parent t)
                                        CRoot     -> 0
env :: AGTree Env
env t =  case constructor t of
                    CRoot      ->  dclo t
                    CBlock     ->  env (parent t)
                    CUse       ->  env (parent t)
                    CDecl      ->  env (parent t)
                    _          ->  case (constructor $ parent t) of
                                                CBlock    -> dclo t
                                                CConsIts  -> env (parent t)
                                                CRoot     -> dclo t


block :: P -> Errors
block p = errors (mkAG p)
