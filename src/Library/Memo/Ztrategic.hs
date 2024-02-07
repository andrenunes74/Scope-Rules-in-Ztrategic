{-# LANGUAGE Rank2Types,DeriveDataTypeable #-}
module Library.Memo.Ztrategic where

import Language.Grammars.ZipperAG
import Data.Generics.Zipper hiding (left, right, up, down')
import Data.Generics.Aliases
import Data.Maybe
import Data.Data
import Control.Monad -- (join, mplus, MonadPlus)

import Library.StrategicData
import Library.Memo.AGMemo

----------
----
--- TP
----
----------

type TP a = Zipper a -> Maybe (Zipper a)

applyTP :: Memoizable d m => TP (d m) -> Zipper (d m) -> Maybe (Zipper (d m))
applyTP f z = fmap cleanMemoTable $ f z

applyTP_unclean :: TP (d m) -> Zipper (d m) -> Maybe (Zipper (d m))
applyTP_unclean f z = f z 

full_tdTP :: StrategicData (d m) => TP (d m) -> TP (d m) 
full_tdTP f = allTPdown (full_tdTP f) `seqTP` allTPright (full_tdTP f) `seqTP` f 

full_buTP :: StrategicData (d m) => TP (d m) -> TP (d m) 
full_buTP f = f `seqTP` allTPright (full_buTP f) `seqTP` allTPdown (full_buTP f) 

once_tdTP :: StrategicData (d m) => TP (d m) -> TP (d m) 
once_tdTP f =  oneTPdown (once_tdTP f) `choiceTP` oneTPright (once_tdTP f) `choiceTP` f

once_buTP :: StrategicData (d m) => TP (d m) -> TP (d m)   
once_buTP f = f `choiceTP` oneTPright (once_buTP f) `choiceTP` oneTPdown (once_buTP f) 

--https://hackage.haskell.org/package/ZipperAG-0.9/docs/Language-Grammars-ZipperAG-Examples-Algol68.html
--change use to decl

--Experimental
stop_tdTP :: StrategicData (d m) => TP (d m) -> TP (d m)   
stop_tdTP f = oneTPdown (stop_tdTP f) `choiceTP` f `seqTP` oneTPright (stop_tdTP f)

--Experimental
stop_buTP :: StrategicData (d m) => TP (d m) -> TP (d m)   
stop_buTP f = oneTPright (stop_buTP f) `seqTP` (f  `choiceTP` oneTPdown (stop_buTP f))

adhocTP :: Typeable a => TP (d m) -> (a -> Maybe a) -> TP (d m)
adhocTP f g = maybeKeep f (zTryApplyM g) 

adhocTPZ :: Typeable a => TP (d m) -> (a -> Zipper (d m) -> Maybe (Zipper (d m))) -> TP (d m)
adhocTPZ f g = maybeKeep f (zTryApplyMZ g)

--Identity function
idTP :: TP (d m)
idTP a = return a

--Failing function
failTP :: TP (d m) 
failTP = const mzero



maybeKeep :: (a -> Maybe a) -> (a -> Maybe a) -> a -> Maybe a
maybeKeep x y z =  case y z of 
              Nothing -> x z 
              Just r -> case x r of 
                  Nothing ->  Just r
                  k -> k 

allTPright :: StrategicData (d m) => TP (d m) -> TP (d m) 
allTPright f z = case right z of 
         Nothing -> return z
         Just r  -> (fromJust . left) <$> f r

--To down node only
allTPdown :: StrategicData (d m) => TP (d m) -> TP (d m)
allTPdown f z = case down' z of 
            Nothing -> return z
            Just d  -> (fromJust . up) <$> f d

--To right node only
oneTPright :: StrategicData (d m) => TP (d m) -> TP (d m)
oneTPright f z = case right z of 
         Nothing -> Nothing
         Just r  -> (fromJust . left) <$> f r

--To down node only
oneTPdown :: StrategicData (d m) => TP (d m) -> TP (d m)
oneTPdown f z = case down' z of 
            Nothing -> Nothing
            Just d  -> (fromJust . up) <$> f d  

--Sequential composition, ignores failure
seqTP :: TP (d m) -> TP (d m) -> TP (d m)
seqTP x y z = maybeKeep x y z
f `mseq` g      =  \x -> g x >>= f

--Sequential composition, chooses rightmost only if possible
choiceTP :: TP (d m) -> TP (d m) -> TP (d m)
choiceTP x y z = maybe (x z) (return . id) (y z)

--Apply a function, fail the composition if it fails
monoTP :: Typeable a => (a -> Maybe a) -> TP (d m)
monoTP = adhocTP failTP

--Apply a function with access to the zipper, fail the composition if it fails
monoTPZ :: Typeable a => (a -> Zipper (d m) -> Maybe (Zipper (d m))) -> TP (d m)
monoTPZ = adhocTPZ failTP

--Try to apply a zipper function, and apply identity if it fails
tryTP :: TP (d m) -> TP (d m)
tryTP s = idTP `choiceTP` s

repeatTP :: TP (d m) -> TP (d m)
repeatTP s =  tryTP (mseq (repeatTP s) s)

innermost :: StrategicData (d m) => TP (d m) -> TP (d m)
innermost s =  (tryTP ((innermost s) `mseq` s))
                   `seqTP` oneTPright (innermost s) `seqTP` oneTPdown (innermost s)

innermost'  :: StrategicData (d m) => TP (d m) -> TP (d m)
innermost' s  =  repeatTP (once_buTP s)

outermost  :: StrategicData (d m) => TP (d m) -> TP (d m)
outermost s  =  repeatTP (once_tdTP s)

----------
----
--- TU
----
----------

type TU m d = (forall a. Zipper a -> (m d, Zipper a)) 

applyTU :: (Memoizable dtype memo) => (Zipper (dtype memo) -> (m d, Zipper (dtype memo))) -> Zipper (dtype memo) -> (m d, Zipper (dtype memo)) 
applyTU f z = case f z of 
                (r, z1) -> (r, cleanMemoTable z1)

applyTU_unclean :: (Zipper (dtype memo) -> (m d, Zipper (dtype memo))) -> Zipper (dtype memo) -> (m d, Zipper (dtype memo)) 
applyTU_unclean f z = f z 

foldr1TU :: (Monoid (m d), StrategicData (dtype memo), Foldable m) => 
    (Zipper (dtype memo) -> (m d, Zipper (dtype memo))) -> 
    Zipper (dtype memo) -> (d -> d -> d) -> d
foldr1TU f z red = foldr1 red $ fst (full_tdTU f z) 

foldl1TU :: (Monoid (m d), StrategicData (dtype memo), Foldable m) => 
    (Zipper (dtype memo) -> (m d, Zipper (dtype memo))) -> 
    Zipper (dtype memo) -> (d -> d -> d) -> d
foldl1TU f z red = foldl1 red $ fst (full_tdTU f z) 

foldrTU :: (Monoid (m d), StrategicData (dtype memo), Foldable m) => 
    (Zipper (dtype memo) -> (m d, Zipper (dtype memo))) -> 
    Zipper (dtype memo) -> (d -> c -> c) -> c -> c
foldrTU f z red i = foldr red i $ fst (full_tdTU f z) 

foldlTU :: (Monoid (m d), StrategicData (dtype memo), Foldable m) => 
    (Zipper (dtype memo) -> (m d, Zipper (dtype memo))) -> 
    Zipper (dtype memo) -> (c -> d -> c) -> c -> c
foldlTU f z red i = foldl red i $ fst (full_tdTU f z) 

full_tdTU :: (Monoid (m d), StrategicData (dtype memo)) => (Zipper (dtype memo) -> (m d, Zipper (dtype memo))) -> (Zipper (dtype memo) -> (m d, Zipper (dtype memo)))   
full_tdTU f = allTUright (full_buTU f) `seqTU` allTUdown (full_buTU f) `seqTU` f

full_buTU :: (Monoid (m d), StrategicData (dtype memo)) => (Zipper (dtype memo) -> (m d, Zipper (dtype memo))) -> (Zipper (dtype memo) -> (m d, Zipper (dtype memo)))
full_buTU f = f `seqTU` allTUdown (full_tdTU f) `seqTU` allTUright (full_tdTU f) 

once_tdTU :: (MonadPlus m, Monoid (m d), StrategicData (dtype memo)) => (Zipper (dtype memo) -> (m d, Zipper (dtype memo))) -> (Zipper (dtype memo) -> (m d, Zipper (dtype memo))) 
once_tdTU f = f `choiceTU` allTUdown (once_tdTU f)  `choiceTU` allTUright (once_tdTU f) 

once_buTU :: (MonadPlus m, Monoid (m d), StrategicData (dtype memo)) => (Zipper (dtype memo) -> (m d, Zipper (dtype memo))) -> (Zipper (dtype memo) -> (m d, Zipper (dtype memo)))
once_buTU f = allTUright (once_buTU f) `choiceTU` allTUdown (once_buTU f) `choiceTU` f

stop_tdTU :: (MonadPlus m, Monoid (m d), StrategicData (dtype memo)) => (Zipper (dtype memo) -> (m d, Zipper (dtype memo))) -> (Zipper (dtype memo) -> (m d, Zipper (dtype memo)))
stop_tdTU f = (f `choiceTU` allTUdown (stop_tdTU f)) `seqTU` allTUright (stop_tdTU f)

stop_buTU :: (MonadPlus m, Monoid (m d), StrategicData (dtype memo)) => (Zipper (dtype memo) -> (m d, Zipper (dtype memo))) -> (Zipper (dtype memo) -> (m d, Zipper (dtype memo)))
stop_buTU f = allTUright (stop_buTU f) `seqTU` (allTUdown (stop_buTU f) `choiceTU` f)

allTUdown :: (Monoid (m d), StrategicData (dtype memo)) => (Zipper (dtype memo) -> (m d, Zipper (dtype memo))) -> (Zipper (dtype memo) -> (m d, Zipper (dtype memo)))
allTUdown f z = case down' z of 
         Nothing -> (mempty, z)
         Just d  -> let (v, z') = f d
                    in (v, fromJust $ up z')

allTUright :: (Monoid (m d), StrategicData (dtype memo)) => (Zipper (dtype memo) -> (m d, Zipper (dtype memo))) -> (Zipper (dtype memo) -> (m d, Zipper (dtype memo)))
allTUright f z = case right z of 
         Nothing -> (mempty, z)
         Just r  -> let (v, z') = f r
                    in (v, fromJust $ left z')

adhocTU :: (Monad m, Typeable a) => (Zipper c -> (m d, Zipper c)) -> (a -> m d) -> (Zipper c -> (m d, Zipper c))
adhocTU f g z = case (zTryReduceM g z) of 
            (Just r, z') -> (r, z')
            (Nothing, z') -> f z'

adhocTUZ :: (Monad m, Typeable a) => (Zipper c -> (m d, Zipper c)) -> (a -> Zipper c -> (m d, Zipper c)) -> (Zipper c -> (m d, Zipper c))
adhocTUZ f g z = case (zTryReduceMZ g z) of 
            (Just r, z') -> (r, z')
            (Nothing, z') -> f z'

seqTU :: (Monoid (m d)) => (Zipper (dtype memo) -> (m d, Zipper (dtype memo)))-> (Zipper (dtype memo) -> (m d, Zipper (dtype memo))) -> (Zipper (dtype memo) -> (m d, Zipper (dtype memo)))
seqTU x y z =  
  let (yr, z') = y z 
      (xr, z'') = x z'
  in (xr `mappend` yr, z'')

--Sequential composition 
choiceTU :: (MonadPlus m) => (Zipper (dtype memo) -> (m d, Zipper (dtype memo))) -> (Zipper (dtype memo) -> (m d, Zipper (dtype memo))) -> (Zipper (dtype memo) -> (m d, Zipper (dtype memo)))
choiceTU x y z = 
  let (yr, z') = y z 
      (xr, z'') = x z'
  in (xr `mplus` yr, z'')

failTU :: (MonadPlus m) => (Zipper (dtype memo) -> (m d, Zipper (dtype memo)))
failTU z = (mzero, z)

constTU :: Monad m => d -> (Zipper (dtype memo) -> (m d, Zipper (dtype memo)))
constTU v z = (return v, z)

--Apply a function, fail the composition if it fails
monoTU :: (MonadPlus m, Typeable a) => (a -> m d) -> (Zipper (dtype memo) -> (m d, Zipper (dtype memo)))
monoTU = adhocTU failTU

--TODO: fix type signature
--Apply a function with access to the zipper, fail the composition if it fails
monoTUZ :: (MonadPlus m, Typeable a) => (a -> Zipper (dtype memo) -> (m d, Zipper (dtype memo))) -> (Zipper (dtype memo) -> (m d, Zipper (dtype memo)))
monoTUZ = adhocTUZ failTU


--TODO: fix type signature
--TODO: improve using Data.Generics.Aliases, fitting mkT or mkM inside of transM instead of chaining casts 
--elevate a reduction, which can access the zipper, to the zipper level. If the type does not match, returns Nothing 
zTryReduceMZ :: (Typeable a) => (a -> Zipper c -> (b, Zipper c)) -> (Zipper c -> (Maybe b, Zipper c))
zTryReduceMZ f z = case getHole z of 
       Nothing -> (Nothing     , z)
       Just r  -> let (x, z') = f r z 
                  in (Just x, z')


--elevate a reduction to the zipper level. If the type does not match, returns Nothing 
zTryReduceM :: (Typeable a) => (a -> b) -> (Zipper e -> (Maybe b, Zipper e)) -- TU Maybe b
zTryReduceM f z = case getHole z of 
       Nothing -> (Nothing   , z)
       Just r  -> (Just (f r), z)


zTryApplyMZ :: (Typeable a) => (a -> Zipper c -> Maybe (Zipper c)) -> TP c
zTryApplyMZ f z = case getHole z of 
                    Nothing -> Nothing 
                    Just v  -> case (f v z) of 
                                Nothing -> Nothing 
                                Just r  -> Just r

--elevate a transformation to the zipper level. If the type does not match, returns Nothing 
zTryApplyM :: (Typeable a, Typeable b) => (a -> Maybe b) -> TP (c)
zTryApplyM f = transM (join . cast . f . fromJust . cast)

--elevate a transformation to the zipper level. If the type does not match, returns Nothing 
zTryApply :: (Typeable a, Typeable b) => (a -> b) -> TP c
zTryApply f = transM (cast . f . fromJust . cast)

--elevate a transformation to the zipper level. If the type does not match, the zipper remains unchanged
zApply :: (Typeable a, Typeable b) => (a -> b) -> (Zipper c -> Zipper c)
zApply f z = maybe z id (zTryApply f z) 