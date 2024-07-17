{-# LANGUAGE DeriveDataTypeable #-}

module Let_Interface where
import qualified Block.Shared as B
import qualified IScopes as I 
import qualified Let.Shared as L 
import qualified Let.SharedAG as LS
import qualified Block.Block_Zippers as M
import qualified Block.SharedAG as BS
import Data.Data
import Data.Generics.Zipper
import Library.StrategicData (StrategicData(..), isJust)
import Library.Ztrategic
import Data.Generics.Aliases
import Library.ZipperAG
import Data.Maybe hiding (isJust)
import Debug.Trace

instance I.Scopes (L.Let) where
    isDecl ag = case (LS.constructor ag) of
        LS.CAssign -> True
        LS.CNestedLet -> True
        _ -> False
    isUse ag = case (LS.constructor ag) of
        LS.CVar -> True
        _ -> False
    isBlock ag = case (LS.constructor ag) of
        LS.CLet -> True
        _ -> False
    isGlobal ag = False
    initialState ag = ["c"]

instance StrategicData (L.Let) where
    isTerminal t = isJust (getHole t :: Maybe Int)
                || isJust (getHole t :: Maybe LS.Name)