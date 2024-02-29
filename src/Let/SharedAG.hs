module Let.SharedAG ( 
                module Let.Shared, 
                Constructor(..), 
                constructor, 
                lexeme_Name,
--                lexeme_Assign, 
                lexeme_Exp,
--                lexeme_NestedLet, 
--                lexeme_Var, 
--                lexeme_Const, 
                AGTree, 
                mkAG) 
        where 

--import Language.Grammars.ZipperAG
import Data.Generics.Zipper
import Data.Data
import Let.Shared
import Debug.Trace

data Constructor = CRoot
                 | CLet
                 | CNestedLet
                 | CAssign
                 | CEmptyList
                 | CAdd
                 | CSub
                 | CNeg
                 | CConst
                 | CVar
                deriving Show

-- --------
-- --
-- - Ags Boilerplate Code
-- --
-- --------

lexeme_Name :: Zipper a -> Name
lexeme_Name ag = case (getHole ag :: Maybe List) of
                          Just (Assign    v _ _) -> v
                          Just (NestedLet v _ _) -> v
                          _ -> case (getHole ag :: Maybe Exp) of
                                      Just (Var s) -> s 
                                      _ -> error "Error in lexeme_Name!"
lexeme_Exp :: Zipper a -> Maybe Exp
lexeme_Exp ag = case (getHole ag :: Maybe List) of
                          Just(Assign _ e _) -> Just e
                          _ -> Nothing

{-
lexeme_Assign :: Zipper a -> String
lexeme_Assign ag = case (getHole ag :: Maybe List) of
                          Just(Assign v _ _) -> v
                          _ -> error "Error in lexeme_Assign!"

lexeme_NestedLet :: Zipper a -> String
lexeme_NestedLet ag = case (getHole ag :: Maybe List) of
                       Just(NestedLet v _ _) -> v
                       _ -> error "Error in lexeme_NestedLet!"

lexeme_Var :: Zipper a -> String
lexeme_Var ag = case (getHole ag :: Maybe Exp) of
                  Just (Var s) -> s
                  _ -> error "Error in lexeme_Var!"

lexeme_Const :: Zipper a -> Int
lexeme_Const ag = case (getHole ag :: Maybe Exp) of
                  Just (Const s) -> s
                  _ -> error "Error in lexeme_Const!"
-}

constructor :: Zipper a -> Constructor
constructor ag = case (getHole ag :: Maybe Root) of
                   Just (Root _) -> CRoot
                   _ -> case (getHole ag :: Maybe Let) of
                          Just (Let _ _) -> CLet
                          _ -> case (getHole ag :: Maybe List) of
                                 Just (NestedLet _ _ _   ) -> CNestedLet
                                 Just (Assign _ _ _) -> CAssign
                                 Just (EmptyList       ) -> CEmptyList
                                 _ -> case (getHole ag :: Maybe Exp) of
                                        Just (Add _ _) -> CAdd
                                        Just (Sub _ _) -> CSub
                                        Just (Neg _  ) -> CNeg
                                        Just (Var _  ) -> CVar
                                        Just (Const _) -> CConst
                                        _              -> error "Error in constructor!!"


type AGTree a  = Zipper Root -> a

mkAG :: Data t => t -> Zipper t
mkAG = toZipper