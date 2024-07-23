module BlockC (block_io) where
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
                       BS.CDecl     -> mustNotBeIn (lev t) (BS.lexeme t) (fromJust $ getHole t) (dcli a t) a

dcli :: BS.Env -> Zipper BS.P -> BS.Env
dcli a t =  case BS.constructor t of
                    BS.CRoot     -> a
                    BS.CNilIts   -> case  (BS.constructor $ parent t) of
                                             BS.CConsIts  -> dclo a (t.$<1)
                                             BS.CBlock    -> dcli a (parent t)
                                             BS.CRoot     -> []
                    BS.CConsIts  -> case  (BS.constructor $ parent t) of
                                             BS.CConsIts  -> dclo a (t.$<1)
                                             BS.CBlock    -> dcli a (parent t)
                                             BS.CRoot     -> []
                    BS.CBlock    -> dcli a (parent t)
                    BS.CUse      -> dcli a (parent t)
                    BS.CDecl     -> dcli a (parent t)

lev :: BS.AGTree Int
lev t =  case BS.constructor t of
            BS.CRoot     ->  0
            BS.CBlock    -> (lev (parent t)) + 1
            BS.CConsIts  -> lev (parent t)

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

mustNotBeInE :: Int -> BS.Name ->  BS.It -> BS.Env -> BS.Errors
mustNotBeInE i name item e =
    let names = (map (\(name', lev, _) -> (name',lev)) e)
        errorMsg = " <= [Duplicated declaration!]"
    in
    if ((name,i) `elem` names) then [(name, item, errorMsg)] else []

mustNotBeInA :: BS.Name -> BS.It -> BS.Env -> BS.Errors
mustNotBeInA name item a =
    let items = filter (\(name', _, item') -> name' == name && item /= item') a
        errorMsg = " <= [Different declaration!]"
    in
    if not (null items) then [(name, item, errorMsg)] else []

mustNotBeIn :: Int -> BS.Name -> BS.It -> BS.Env -> BS.Env -> BS.Errors
mustNotBeIn i name item e a =
    mustNotBeInE i name item e ++ mustNotBeInA name item a

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

block_io :: BS.Env -> BS.P -> B.Errors
block_io a p = errors a (mkAG p)