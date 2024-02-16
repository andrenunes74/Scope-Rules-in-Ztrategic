module Block.Block_Strategies where

import Library.ZipperAG
import Data.Generics.Zipper
import Data.Maybe
-- import Data.Data
import Library.Ztrategic
import Library.StrategicData

import Block.Shared

instance StrategicData P where 
  isTerminal t = False  

runZtrategic :: (Zipper P -> a) -> P -> a
runZtrategic st = st . toZipper

--  Defined Names


-- TopDown

decl :: It -> Maybe [String]
decl (Decl n)  = return [n]
decl _         = return []


-- definedNamesTD  :: Zipper P -> [String]
definedNamesTD  :: Zipper P -> Maybe [String]
definedNamesTD  r = applyTU (full_tdTU step) r 
       where step = failTU `adhocTU` decl


-- Result:
-- *Main> runZtrategic definedNamesTD program
-- ["y","x","x","x"]



-- OncetopDown

decl' :: It -> Maybe [String]
decl' (Decl n)  = Just [n]
decl' _         = Just []

definedNamesOTD  :: Zipper P -> Maybe [String]
definedNamesOTD  r = applyTU (once_tdTU step) r 
       where step = failTU `adhocTU` decl'


-- if we use decl we have a full_tp traversal
-- Result: 
-- *Main> runZtrategic definedNamesOTD program
-- ["y"]






-- BottomUp


definedNamesBU  :: Zipper P -> Maybe [String]
definedNamesBU  r = applyTU (full_buTU step) r 
       where step = failTU `adhocTU` decl

-- *Main> runZtrategic definedNamesBU program
-- ["x","x","x","y"]


-- InnerMost

declX :: It -> Maybe It
declX (Decl n)  | n == "x"  = Just (Decl "xx")
--                | otherwise = Just (Decl n)     -- doesn't terminate
                | otherwise = Nothing
declX _         = Nothing


replaceXIM  :: Zipper P -> Maybe (Zipper P)
replaceXIM  r = applyTP (innermost step) r 
       where step = failTP `adhocTP` declX


runReplaceXIM :: P -> Maybe P
runReplaceXIM p = case replaceXIM (toZipper p) of
                        Nothing ->  Nothing
                        _       ->  getHole $ fromJust $ replaceXIM (toZipper p)



-- Full TopDown


replaceXTD  :: Zipper P -> Maybe (Zipper P)
replaceXTD  r = applyTP (full_tdTP step) r 
       where step = failTP `adhocTP` declX



runReplaceXTD :: P -> Maybe P
runReplaceXTD p = case replaceXTD (toZipper p) of
                        Nothing ->  Nothing
                        _       ->  getHole $ fromJust $ replaceXTD (toZipper p)

--
-- *Main> runReplaceXTD program
-- Just [ Decl y,Decl xx,[ Decl xx,Use y,Use w ],Decl xx,Use y ]


-- Once TopDown

replaceXOTD  :: Zipper P -> Maybe (Zipper P)
replaceXOTD  r = applyTP (once_tdTP step) r 
       where step = failTP `adhocTP` declX



runReplaceXOTD :: P -> Maybe P
runReplaceXOTD p = case replaceXOTD (toZipper p) of
                        Nothing ->  Nothing
                        _       ->  getHole $ fromJust $ replaceXOTD (toZipper p)

-- *Main> runReplaceXOTD program
-- Just [ Decl y,Decl xx,[ Decl x,Use y,Use w ],Decl x,Use y ]




-- Once BottomUp

replaceXOBU  :: Zipper P -> Maybe (Zipper P)
replaceXOBU  r = applyTP (once_buTP step) r 
       where step = failTP `adhocTP` declX



runReplaceXOBU :: P -> Maybe P
runReplaceXOBU p = case replaceXOBU (toZipper p) of
                        Nothing ->  Nothing
                        _       ->  getHole $ fromJust $ replaceXOBU (toZipper p)


-- *Main> runReplaceXOBU program
-- Just [ Decl y,Decl x,[ Decl xx,Use y,Use w ],Decl x,Use y ]

