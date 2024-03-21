module Examples.AG.PP.SharedAG where


import Data.Data
import Data.Generics.Zipper
import Language.Grammars.ZipperAG hiding ((.|))
import Examples.AG.PP.Shared


data Constructor = CEmpty
                 | CText
                 | CIndent
                 | CBeside
                 | CAbove
                 | CDup
                 | CJoin
                 ---------------
                 | CApply
                 | CParC
                 | CIndentC
                 | CBesideC
                 | CAboveC
                 | CDupC
                 | CJoinC
                 ---------------
                 | CNilArgs
                 | CConsArgs
                 ---------------
                 | CApplyC
                 | CNilPPCArgs
                 | CConsPPCArgs
                 ---------------
                 | CFilla
                 | CFillBlock
                 | CConsFillList
                 | CNilFillList
                 ---------------
                 | CBest
                 | CAll
                 | CDispl
                 ---------------
                deriving (Eq,Show)

constructor :: (Typeable a) => Zipper a -> Constructor
constructor a = case ( getHole a :: Maybe PPRoot ) of
  Just (Best _) -> CBest
  Just (All _) -> CAll
  Just (Displ _) -> CDispl
  _ -> case ( getHole a :: Maybe PPS ) of
    Just Empty -> CEmpty
    Just (Text _) -> CText
    Just (Indent _ _) -> CIndent
    Just (Beside _ _) -> CBeside
    Just (Above _ _) -> CAbove
    Just (Dup _ _) -> CDup
    Just (Join _) -> CJoin
    -----------------------------------------------
    Just (Apply _ _) -> CApply
    -----------------------------------------------
    Just (Filla _) -> CFilla
    Just (FillBlock _ _) -> CFillBlock
    -----------------------------------------------
    _ -> case ( getHole a :: Maybe PPC ) of
      Just ParC -> CParC
      Just (IndentC _ _) -> CIndentC
      Just (BesideC _ _) -> CBesideC
      Just (AboveC _ _) -> CAboveC
      Just (DupC _ _) -> CDupC
      Just (JoinC _) -> CJoinC
      ----------------------------------------------
      Just (ApplyC _ _) -> CApplyC
      ----------------------------------------------
      _ -> case ( getHole a :: Maybe PPSArgs ) of
        Just NilArgs -> CNilArgs
        Just (ConsArgs _ _) -> CConsArgs
        --------------------------------------------
        _ -> case ( getHole a :: Maybe PPCArgs ) of
          Just NilPPCArgs -> CNilPPCArgs
          Just (ConsPPCArgs _ _) -> CConsPPCArgs
          -------------------------------------------
          _ -> case ( getHole a :: Maybe FillList ) of
            Just NilFillList -> CNilFillList
            Just (ConsFillList _ _) -> CConsFillList
            _ -> error "That production does not exist!"


lexemeT :: Zipper PPRoot -> String
lexemeT z = case (getHole z :: Maybe PPS) of
            Just (Text x) -> x
            _              -> error "Unexpected use of lexeme!"

lexemeI :: Zipper PPRoot -> Int
lexemeI z = case (getHole z :: Maybe PPS) of
            Just (Indent x _) -> x
            _ -> case (getHole z :: Maybe PPC) of
              Just (IndentC x _) -> x
              _              -> error "Unexpected use of lexeme!"

lexemeF :: Zipper PPRoot -> Int
lexemeF z = case (getHole z :: Maybe PPS) of
            Just (FillBlock x _) -> x
            _              -> error "Unexpected use of lexeme!"

-----------------------------------------------------

(.|) :: Zipper a -> Int -> Bool
z .| 1 = case left z of
  Nothing -> False
  _ -> True
z .| n = case left z of
  Nothing -> False
  Just x ->  x .| (n-1)

(.$>) :: Zipper a -> Int -> Zipper a
zipper .$> n = let current = aux zipper 1
               in  (parent zipper).$(current+n)

(.$<) :: Zipper a -> Int -> Zipper a
zipper .$< n = let current = aux zipper 1
               in  (parent zipper).$(current-n)


aux :: Zipper a -> Int -> Int
aux m n = case left m of
                     Nothing  -> n
                     Just m'  -> aux m' (n+1)