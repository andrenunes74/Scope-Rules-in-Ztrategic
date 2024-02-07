{-# LANGUAGE Rank2Types,DeriveDataTypeable #-}
module Library.Memo.State.Ztrategic where

import Language.Grammars.ZipperAG
import Data.Generics.Zipper hiding (left, right, up, down')
import Data.Generics.Aliases
import Data.Maybe
import Data.Data
import Control.Monad -- (join, mplus, MonadPlus)
import Control.Monad.State.Lazy

import Library.StrategicData

data Result = Success | Fail deriving Show 
andS Success Success = Success
andS _ _ = Fail
orS Fail Fail = Fail 
orS _ _ = Success

----------
----
--- TP
----
----------

type TP a = State (Zipper a) Result

applyTP :: TP c -> Zipper c -> Zipper c
applyTP f z = execState f z

full_tdTP :: StrategicData a => TP a -> TP a
full_tdTP f = allTPdown (full_tdTP f) `seqTP` allTPright (full_tdTP f) `seqTP` f 

full_buTP :: StrategicData a => TP a -> TP a
full_buTP f = f `seqTP` allTPright (full_buTP f) `seqTP` allTPdown (full_buTP f) 

once_tdTP :: StrategicData a => TP a -> TP a
once_tdTP f =  oneTPdown (once_tdTP f) `choiceTP` oneTPright (once_tdTP f) `choiceTP` f

once_buTP :: StrategicData a => TP a -> TP a  
once_buTP f = f `choiceTP` oneTPright (once_buTP f) `choiceTP` oneTPdown (once_buTP f) 

--Experimental
stop_tdTP :: StrategicData a => TP a -> TP a
stop_tdTP f = oneTPdown (stop_tdTP f) `choiceTP` f `seqTP` oneTPright (stop_tdTP f)

--Experimental
stop_buTP :: StrategicData a => TP a -> TP a
stop_buTP f = oneTPright (stop_buTP f) `seqTP` (f  `choiceTP` oneTPdown (stop_buTP f))

adhocTP :: Typeable a => TP e -> (a -> Maybe a) -> TP e
adhocTP f g = f `seqTP` (zTryApplyM g) 

adhocTPZ :: Typeable a => TP e -> (a -> Zipper e -> Maybe (Zipper e)) -> TP e
adhocTPZ f g = f `seqTP` (zTryApplyMZ g)

--Identity function
idTP :: TP a
idTP = return Success

--Failing function
failTP :: TP a 
failTP = return Fail


--To right node only
allTPright :: StrategicData a => TP a -> TP a
allTPright f = do
            z <- get 
            case right z of 
              Nothing -> return Success
              Just r  -> do 
                         put r 
                         res <- f 
                         modify (fromJust . left)
                         return res

--To down node only
allTPdown :: StrategicData a => TP a -> TP a
allTPdown f = do
            z <- get 
            case down' z of 
              Nothing -> return Success
              Just d  -> do 
                  put d
                  res <- f 
                  modify (fromJust . up)
                  return res

--To right node only
oneTPright :: StrategicData a => TP a -> TP a
oneTPright f = do
            z <- get 
            case right z of 
              Nothing -> return Fail
              Just r  -> do 
                         put r 
                         res <- f 
                         modify (fromJust . left)
                         return res

--To down node only
oneTPdown :: StrategicData a => TP a -> TP a
oneTPdown f = do
            z <- get 
            case down' z of 
              Nothing -> return Fail
              Just d  -> do 
                  put d
                  res <- f 
                  modify (fromJust . up)
                  return res


--Sequential composition, ignores failure
seqTP :: TP a -> TP a -> TP a
seqTP x y = do 
          r1 <- y
          r2 <- x
          return (orS r1 r2)

mseq :: TP a -> TP a -> TP a
f `mseq` g      =  do 
  r1 <- g
  case r1 of 
    Fail -> return Fail 
    Success -> f 

--Sequential composition, chooses rightmost only if possible
choiceTP :: TP a -> TP a -> TP a
choiceTP x y = do 
  r1 <- y 
  case r1 of 
   Success -> return Success 
   Fail -> x

--Apply a function, fail the composition if it fails
monoTP :: Typeable a => (a -> Maybe a) -> TP e
monoTP = adhocTP failTP

--Apply a function with access to the zipper, fail the composition if it fails
monoTPZ :: Typeable a => (a -> Zipper e -> Maybe (Zipper e)) -> TP e
monoTPZ = adhocTPZ failTP

--Try to apply a zipper function, and apply identity if it fails
tryTP :: TP a -> TP a
tryTP s = idTP `choiceTP` s

repeatTP :: TP a -> TP a
repeatTP s =  tryTP (mseq (repeatTP s) s)

innermost :: StrategicData a => TP a -> TP a
innermost s =  (tryTP ((innermost s) `mseq` s))
                   `seqTP` oneTPright (innermost s) `seqTP` oneTPdown (innermost s)

innermost'  :: StrategicData a => TP a -> TP a
innermost' s  =  repeatTP (once_buTP s)

outermost  :: StrategicData a => TP a -> TP a
outermost s  =  repeatTP (once_tdTP s)

----------
----
--- TU
----
----------

type TU m d = (forall a. State (Zipper a) (m d, Result))

applyTU :: State (Zipper c) (m d, Result) -> Zipper c -> m d 
applyTU f z = fst $ evalState f z 

foldr1TU :: (Monoid (m d), StrategicData c, Foldable m) => 
    State (Zipper c) (m d, Result) -> 
    Zipper c -> (d -> d -> d) -> d
foldr1TU f z red = foldr1 red $ applyTU (full_tdTU f) z 

foldl1TU :: (Monoid (m d), StrategicData c, Foldable m) => 
    State (Zipper c) (m d, Result) -> 
    Zipper c -> (d -> d -> d) -> d
foldl1TU f z red = foldl1 red $ applyTU (full_tdTU f) z 

foldrTU :: (Monoid (m d), StrategicData c, Foldable m) => 
    State (Zipper c) (m d, Result) -> 
    Zipper c -> (d -> v -> v) -> v -> v
foldrTU f z red i = foldr red i $ applyTU (full_tdTU f) z 

foldlTU :: (Monoid (m d), StrategicData c, Foldable m) => 
    State (Zipper c) (m d, Result) -> 
    Zipper c -> (v -> d -> v) -> v -> v
foldlTU f z red i = foldl red i $ applyTU (full_tdTU f) z 

full_tdTU :: (Monoid (m d), StrategicData c) => State (Zipper c) (m d, Result) -> State (Zipper c) (m d, Result)
full_tdTU f = allTUright (full_buTU f) `seqTU` allTUdown (full_buTU f) `seqTU` f

full_buTU :: (Monoid (m d), StrategicData c) => State (Zipper c) (m d, Result) -> State (Zipper c) (m d, Result)
full_buTU f = f `seqTU` allTUdown (full_tdTU f) `seqTU` allTUright (full_tdTU f) 

once_tdTU :: (MonadPlus m, Monoid (m d), StrategicData c) => State (Zipper c) (m d, Result) -> State (Zipper c) (m d, Result)
once_tdTU f = f `choiceTU` allTUdown (once_tdTU f)  `choiceTU` allTUright (once_tdTU f) 

once_buTU :: (MonadPlus m, Monoid (m d), StrategicData c) => State (Zipper c) (m d, Result) -> State (Zipper c) (m d, Result)
once_buTU f = allTUright (once_buTU f) `choiceTU` allTUdown (once_buTU f) `choiceTU` f

stop_tdTU :: (MonadPlus m, Monoid (m d), StrategicData c) => State (Zipper c) (m d, Result) -> State (Zipper c) (m d, Result)
stop_tdTU f = (f `choiceTU` allTUdown (stop_tdTU f)) `seqTU` allTUright (stop_tdTU f)

stop_buTU :: (MonadPlus m, Monoid (m d), StrategicData c) => State (Zipper c) (m d, Result) -> State (Zipper c) (m d, Result)
stop_buTU f = allTUright (stop_buTU f) `seqTU` (allTUdown (stop_buTU f) `choiceTU` f)

allTUdown :: (Monoid (m d), StrategicData c) => State (Zipper c) (m d, Result) -> State (Zipper c) (m d, Result)
allTUdown f = do 
         z <- get 
         case down' z of 
           Nothing -> return (mempty, Success)
           Just d  -> do 
                      put d 
                      res <- f
                      modify (fromJust . up)
                      return res

allTUright :: (Monoid (m d), StrategicData c) => State (Zipper c) (m d, Result) -> State (Zipper c) (m d, Result)
allTUright f = do 
         z <- get 
         case right z of 
           Nothing -> return (mempty, Success)
           Just r  -> do 
                      put r 
                      res <- f
                      modify (fromJust . left)
                      return res

adhocTU :: (Monad m, Typeable a) => State (Zipper c) (m d, Result) -> (a -> m d) -> State (Zipper c) (m d, Result)
adhocTU f g = do 
            r <- zTryReduceM g
            case r of 
              (_, Fail) -> f
              x -> return x

adhocTUZ :: (Monad m, Typeable a) => State (Zipper c) (m d, Result) -> (a -> Zipper c -> (m d, Zipper c)) -> State (Zipper c) (m d, Result) 
adhocTUZ f g = do 
            r <- zTryReduceMZ g
            case r of 
              (_, Fail) -> f
              x -> return x

seqTU :: (Monoid (m d)) => State (Zipper c) (m d, Result)-> State (Zipper c) (m d, Result)  -> State (Zipper c) (m d, Result)
seqTU x y = do 
      (yr, r1) <- y
      (xr, r2) <- x
      return (xr `mappend` yr, orS r1 r2)

--Sequential composition 
choiceTU :: (MonadPlus m) => State (Zipper c) (m d, Result) -> State (Zipper c) (m d, Result)-> State (Zipper c) (m d, Result)
choiceTU x y = do 
      (yr, r1) <- y 
      (xr, r2) <- x
      return (xr `mplus` yr, orS r1 r2)

failTU :: (MonadPlus m) => State (Zipper c) (m d, Result)
failTU = return (mzero, Fail)

constTU :: Monad m => d -> State (Zipper c) (m d, Result)
constTU v = return (return v, Success)

--Apply a function, fail the composition if it fails
monoTU :: (MonadPlus m, Typeable a) => (a -> m d) -> State (Zipper c) (m d, Result)
monoTU = adhocTU failTU

--Apply a function with access to the zipper, fail the composition if it fails
monoTUZ :: (MonadPlus m, Typeable a) => (a -> Zipper c -> (m d, Zipper c)) -> State (Zipper c) (m d, Result)
monoTUZ = adhocTUZ failTU

--elevate a reduction, which can access the zipper, to the zipper level. If the type does not match, returns Nothing 
zTryReduceMZ :: (Typeable a) => (a -> Zipper c -> (m d, Zipper c)) -> State (Zipper c) (m d, Result)
zTryReduceMZ f = do 
       z <- get 
       case getHole z of 
         Nothing -> return (undefined, Fail)
         Just r  -> let (x, z') = f r z 
                    in do 
                       put z'
                       return (x, Success)


--elevate a reduction to the zipper level. If the type does not match, returns Nothing 
zTryReduceM :: (Typeable a) => (a -> m d) -> TU m d
zTryReduceM f = do 
       z <- get 
       case getHole z of 
         Nothing -> return (undefined, Fail   )
         Just r  -> return (f r      , Success)


--elevate a transformation to the zipper level. If the type does not match, returns Nothing 
zTryApplyMZ :: (Typeable a) => (a -> Zipper c -> Maybe (Zipper c)) -> TP c
zTryApplyMZ f = do 
        z <- get 
        case getHole z of 
          Nothing -> return Fail 
          Just v  -> case (f v z) of 
                      Nothing -> return Fail 
                      Just r  -> do 
                                put r 
                                return Success

--elevate a transformation to the zipper level. If the type does not match, returns Nothing 
zTryApplyM :: (Typeable a, Typeable b) => (a -> Maybe b) -> TP c
zTryApplyM f = do 
        z <- get 
        case transM (join . cast . f . fromJust . cast) z of 
         Nothing -> return Fail
         Just z' -> do 
            put z' 
            return Success