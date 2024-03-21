{-# LANGUAGE GADTs,
             MultiParamTypeClasses,
             FlexibleContexts,
             FlexibleInstances, 
             DeriveDataTypeable
#-}

module PP.Memo.SharedMemo where

import Data.Generics.Zipper
import Data.Data

import Library.Memo.AGMemo
import Library.StrategicData


         -- PPSRoot -- 
data PPS m = Best (PPS m) m
         | All (PPS m) m
         | Displ (PPS m) m
         -- PPS -- 
         | Empty m 
         | Text String m 
         | Indent Int (PPS m) m 
         | Beside (PPS m) (PPS m) m 
         | Above (PPS m) (PPS m) m 
         | Dup (PPS m) (PPS m) m 
         | Join (PPS m) m 
         --------------------
         | Apply (PPS m) (PPS m) m
         --------------------
         | Filla (PPS m) m
         | FillBlock Int (PPS m) m
         -- PPC -- 
         | ParC m
         | IndentC Int (PPS m) m
         | BesideC (PPS m) (PPS m) m
         | AboveC (PPS m) (PPS m) m
         | DupC (PPS m) (PPS m) m
         | JoinC (PPS m) m
         --------------------
         | ApplyC (PPS m) (PPS m) m
         -- PPSArgs -- 
         | ConsArgs (PPS m) (PPS m) m
         | NilArgs m
         -- PPCArgs -- 
         | ConsPPCArgs (PPS m) (PPS m) m
         | NilPPCArgs m
         -- FillList -- 
         | ConsFillList (PPS m) (PPS m) m
         | NilFillList m 
              deriving (Typeable, Data, Show)

------------------------------------------------------------

type T_PW  = Int
type T_PLL = Int
type T_PH  = Int

type T_MINS = [Sizes]

type Sizes = (T_PW, T_PLL, T_PH)

data T_FRAME = F  T_PW   T_PLL
             deriving (Eq, Show, Typeable, Data)

instance Ord T_FRAME where
  (F w _) <= (F w' _) = w <= w'
  max x@(F w _) y@(F w' _)
    | w > w'    = x
    | otherwise = y

type T_REQS = [T_FRAME]

type Formats = [Format]

data Format = Elem { height  :: T_PH
                   , last_w  :: T_PLL
                   , total_w :: T_PW
                   , txtstr  :: Int -> String -> String
                   }
                deriving (Data, Typeable)

instance Show Format where 
  show (Elem h l t tx) = "Elem " ++ show h ++ " " ++ show l ++ " " ++ show t ++ " " ++ "<function>"

instance Eq Format  where
  x == y =  height x  == height y
         && total_w x == total_w y
         && last_w  x == last_w  y

instance Ord Format where
  x <= y =  height x <= height y
         || (  height x == height y
            && total_w x <= total_w y )
  x <  y =  height x < height y
         || (  height x == height y
            && total_w x < total_w y )

data T_FORMATS = AFormat Formats
               | TFormats Formats Formats T_ERROR T_ERROR
                deriving (Show, Data, Typeable)

type T_FMTS = [T_FORMATS]


type T_ERROR = Bool

type T_ERRS = [T_ERROR]


--------------------------------------------

updMemoTable' :: (m -> m) -> PPS m -> PPS m
updMemoTable' f (Best p m) = Best p (f m)
updMemoTable' f (All p m) = All p (f m)
updMemoTable' f (Displ p m) = Displ p (f m)
updMemoTable' f (Empty m) = Empty (f m)
updMemoTable' f (Text s m) = Text s (f m)
updMemoTable' f (Indent i p m) = Indent i p (f m)
updMemoTable' f (Beside p1 p2 m) = Beside p1 p2 (f m)
updMemoTable' f (Above p1 p2 m) = Above p1 p2 (f m)
updMemoTable' f (Dup p1 p2 m) = Dup p1 p2 (f m)
updMemoTable' f (Join p m) = Join p (f m)
updMemoTable' f (Apply p1 p2 m) = Apply p1 p2 (f m)
updMemoTable' f (ApplyC p1 p2 m) = ApplyC p1 p2 (f m)
updMemoTable' f (Filla fi m) = Filla fi (f m)
updMemoTable' f (FillBlock i fi m) = FillBlock i fi (f m)
updMemoTable' f (ParC m) = ParC (f m)
updMemoTable' f (IndentC i p m) = IndentC i p (f m)
updMemoTable' f (BesideC p1 p2 m) = BesideC p1 p2 (f m)
updMemoTable' f (AboveC p1 p2 m) = AboveC p1 p2 (f m)
updMemoTable' f (DupC p1 p2 m) = DupC p1 p2 (f m)
updMemoTable' f (JoinC p m) = JoinC p (f m)
updMemoTable' f (ConsArgs p1 p2 m) = ConsArgs p1 p2 (f m)
updMemoTable' f (NilArgs m) = NilArgs (f m)
updMemoTable' f (ConsPPCArgs p1 p2 m) = ConsPPCArgs p1 p2 (f m)
updMemoTable' f (NilPPCArgs m) = NilPPCArgs (f m)
updMemoTable' f (ConsFillList fi p m) = ConsFillList fi p (f m)
updMemoTable' f (NilFillList m) = NilFillList (f m)

--- 

getMemoTable' :: PPS m -> m
getMemoTable' (Best p m) = m
getMemoTable' (All p m) = m
getMemoTable' (Displ p m) = m
getMemoTable' (Empty m) = m
getMemoTable' (Text s m) = m
getMemoTable' (Indent i p m) = m
getMemoTable' (Beside p1 p2 m) = m
getMemoTable' (Above p1 p2 m) = m
getMemoTable' (Dup p1 p2 m) = m
getMemoTable' (Join p m) = m
getMemoTable' (Apply p1 p2 m) = m
getMemoTable' (ApplyC p1 p2 m) = m
getMemoTable' (Filla fi m) = m
getMemoTable' (FillBlock i fi m) = m
getMemoTable' (ParC m) = m
getMemoTable' (IndentC i p m) = m
getMemoTable' (BesideC p1 p2 m) = m
getMemoTable' (AboveC p1 p2 m) = m
getMemoTable' (DupC p1 p2 m) = m
getMemoTable' (JoinC p m) = m
getMemoTable' (ConsArgs p1 p2 m) = m
getMemoTable' (NilArgs m) = m
getMemoTable' (ConsPPCArgs p1 p2 m) = m
getMemoTable' (NilPPCArgs m) = m
getMemoTable' (ConsFillList fi p m) = m
getMemoTable' (NilFillList m) = m


--------------------------------------------

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
constructor a = case getHole a :: Maybe (PPS MemoTable) of
  Just (Best _ m) -> CBest
  Just (All _ m) -> CAll
  Just (Displ _ m) -> CDispl
  Just (Empty m) -> CEmpty
  Just (Text _ m) -> CText
  Just (Indent _ _ m) -> CIndent
  Just (Beside _ _ m) -> CBeside
  Just (Above _ _ m) -> CAbove
  Just (Dup _ _ m) -> CDup
  Just (Join _ m) -> CJoin
  -----------------------------------------------
  Just (Apply _ _ m) -> CApply
  -----------------------------------------------
  Just (Filla _ m) -> CFilla
  Just (FillBlock _ _ m) -> CFillBlock
  -----------------------------------------------
  Just (ParC m) -> CParC
  Just (IndentC _ _ m) -> CIndentC
  Just (BesideC _ _ m) -> CBesideC
  Just (AboveC _ _ m) -> CAboveC
  Just (DupC _ _ m) -> CDupC
  Just (JoinC _ m) -> CJoinC
  ----------------------------------------------
  Just (ApplyC _ _ m) -> CApplyC
  -----------------------------------------------
  Just (NilArgs m) -> CNilArgs
  Just (ConsArgs _ _ m) -> CConsArgs
  --------------------------------------------
  Just (NilPPCArgs m) -> CNilPPCArgs
  Just (ConsPPCArgs _ _ m) -> CConsPPCArgs
  -------------------------------------------
  Just (NilFillList m) -> CNilFillList
  Just (ConsFillList _ _ m) -> CConsFillList
  _ -> error "That production does not exist!"


lexemeT :: Zipper (PPS MemoTable) -> String
lexemeT z = case (getHole z :: Maybe (PPS MemoTable)) of
  Just (Text x m) -> x
  _              -> error "Unexpected use of lexeme!"

lexemeI :: Zipper (PPS MemoTable) -> Int
lexemeI z = case (getHole z :: Maybe (PPS MemoTable)) of
  Just (Indent x _ m) -> x
  Just (IndentC x _ m) -> x
  _              -> error "Unexpected use of lexeme!"

lexemeF :: Zipper (PPS MemoTable) -> Int
lexemeF z = case (getHole z :: Maybe (PPS MemoTable)) of
  Just (FillBlock x _ m) -> x
  _              -> error "Unexpected use of lexeme!"

-- PPS


data Att_FMTS  = Att_FMTS
data Att_ERROR = Att_ERROR
data Att_FRAME = Att_FRAME
data Att_MAXH  = Att_MAXH
data Att_MINW  = Att_MINW
data Att_MINLL = Att_MINLL 
data Att_FMTSL  = Att_FMTSL
data Att_FILLFMTS = Att_FILLFMTS
data Att_IFILLFMTS = Att_IFILLFMTS
data Att_NUMPARS = Att_NUMPARS
data Att_LEN = Att_LEN
data Att_FMTSFL = Att_FMTSFL
data Att_FMTSI = Att_FMTSI
data Att_PW = Att_PW
data Att_MINLLI = Att_MINLLI
data Att_MINWI = Att_MINWI
data Att_MAXHI = Att_MAXHI
data Att_FILLERRS = Att_FILLERRS
data Att_IFILLERRS = Att_IFILLERRS
data Att_ERRORL = Att_ERRORL
data Att_ERRORI = Att_ERRORI
data Att_REQS = Att_REQS
data Att_IREQS = Att_IREQS
data Att_FILLMINS = Att_FILLMINS
data Att_MINS = Att_MINS

type MemoTable = ( (Maybe T_FORMATS     -- fmts
                 , Maybe T_ERROR     -- error
                 , Maybe T_FRAME     -- frame
                 , Maybe T_PH     -- maxh
                 , Maybe T_PW  -- minw
                 , Maybe T_PLL  -- minll
                 , Maybe T_FMTS ) , (  -- fmtsL
                  Maybe T_FMTS -- fillfmts
                 , Maybe T_FMTS -- ifillfmts
                 , Maybe Int    -- numPars
                 , Maybe Int    -- len
                 , Maybe Formats -- fmtsFl
                 , Maybe Formats -- fmtsi
                 , Maybe T_PW  ) , ( -- pw
                  Maybe T_PW  -- minlli  
                 , Maybe T_PW  -- minwi   
                 , Maybe T_PH  -- maxhi         
                 , Maybe T_ERRS  -- fillerrs         
                 , Maybe T_ERRS  -- ifillerrs        
                 , Maybe T_ERRS  -- errorl          
                 , Maybe T_ERROR ) , ( -- errori        
                  Maybe T_REQS  -- reqs        
                 , Maybe T_REQS  -- ireqs   
                 , Maybe T_MINS  -- fillmins  
                 , Maybe T_MINS  -- mins             
                 ) )
       

type Testtype = ( (Maybe T_FORMATS     -- fmts
                 , Maybe T_ERROR     -- error
                 , Maybe T_FRAME     -- frame
                 , Maybe T_PH     -- maxh
                 , Maybe T_PW  -- minw
                 , Maybe T_PLL  -- minll
                 , Maybe T_FMTS ) -- fmtsL 
                 , (Maybe T_FMTS ) -- fillfmts
                 ) 

testVal :: Testtype -> Zipper Testtype
testVal = toZipper 

-- Haskell won't derive Show for tuples with more than 15 elements. So we just make a tuple of tuples :)
emptyMemo =((Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing),
            (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing),
            (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing),
            (Nothing,Nothing,Nothing,Nothing))

instance Memo Att_FMTS MemoTable T_FORMATS where
  mlookup _   ((a,_,_,_,_,_,_),(_,_,_,_,_,_,_),(_,_,_,_,_,_,_),(_,_,_,_)) = a
  massign _ v ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n),(o,p,q,r,s,t,u),(w,x,y,z)) = ((Just v,b,c,d,e,f,g),(h,i,j,k,l,m,n),(o,p,q,r,s,t,u),(w,x,y,z))

instance Memo Att_ERROR MemoTable T_ERROR where
  mlookup _   ((_,b,_,_,_,_,_),(_,_,_,_,_,_,_),(_,_,_,_,_,_,_),(_,_,_,_)) = b
  massign _ v ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n),(o,p,q,r,s,t,u),(w,x,y,z)) = ((a,Just v,c,d,e,f,g),(h,i,j,k,l,m,n),(o,p,q,r,s,t,u),(w,x,y,z))

instance Memo Att_FRAME MemoTable T_FRAME where
  mlookup _   ((_,_,c,_,_,_,_),(_,_,_,_,_,_,_),(_,_,_,_,_,_,_),(_,_,_,_)) = c
  massign _ v ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n),(o,p,q,r,s,t,u),(w,x,y,z)) = ((a,b,Just v,d,e,f,g),(h,i,j,k,l,m,n),(o,p,q,r,s,t,u),(w,x,y,z))

instance Memo Att_MAXH MemoTable T_PH where
  mlookup _   ((_,_,_,d,_,_,_),(_,_,_,_,_,_,_),(_,_,_,_,_,_,_),(_,_,_,_)) = d
  massign _ v ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n),(o,p,q,r,s,t,u),(w,x,y,z)) = ((a,b,c,Just v,e,f,g),(h,i,j,k,l,m,n),(o,p,q,r,s,t,u),(w,x,y,z))

instance Memo Att_MINW MemoTable T_PW where
  mlookup _   ((_,_,_,_,e,_,_),(_,_,_,_,_,_,_),(_,_,_,_,_,_,_),(_,_,_,_)) = e
  massign _ v ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n),(o,p,q,r,s,t,u),(w,x,y,z)) = ((a,b,c,d,Just v,f,g),(h,i,j,k,l,m,n),(o,p,q,r,s,t,u),(w,x,y,z))

instance Memo Att_MINLL MemoTable T_PLL where
  mlookup _   ((_,_,_,_,_,f,_),(_,_,_,_,_,_,_),(_,_,_,_,_,_,_),(_,_,_,_)) = f
  massign _ v ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n),(o,p,q,r,s,t,u),(w,x,y,z)) = ((a,b,c,d,e,Just v,g),(h,i,j,k,l,m,n),(o,p,q,r,s,t,u),(w,x,y,z))

instance Memo Att_FMTSL MemoTable T_FMTS where
  mlookup _   ((_,_,_,_,_,_,g),(_,_,_,_,_,_,_),(_,_,_,_,_,_,_),(_,_,_,_)) = g
  massign _ v ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n),(o,p,q,r,s,t,u),(w,x,y,z)) = ((a,b,c,d,e,f,Just v),(h,i,j,k,l,m,n),(o,p,q,r,s,t,u),(w,x,y,z))

instance Memo Att_FILLFMTS MemoTable T_FMTS where
  mlookup _   ((_,_,_,_,_,_,_),(h,_,_,_,_,_,_),(_,_,_,_,_,_,_),(_,_,_,_)) = h
  massign _ v ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n),(o,p,q,r,s,t,u),(w,x,y,z)) = ((a,b,c,d,e,f,g),(Just v,i,j,k,l,m,n),(o,p,q,r,s,t,u),(w,x,y,z))

instance Memo Att_IFILLFMTS MemoTable T_FMTS where
  mlookup _   ((_,_,_,_,_,_,_),(_,i,_,_,_,_,_),(_,_,_,_,_,_,_),(_,_,_,_)) = i
  massign _ v ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n),(o,p,q,r,s,t,u),(w,x,y,z)) = ((a,b,c,d,e,f,g),(h,Just v,j,k,l,m,n),(o,p,q,r,s,t,u),(w,x,y,z))

instance Memo Att_NUMPARS MemoTable Int where
  mlookup _   ((_,_,_,_,_,_,_),(_,_,j,_,_,_,_),(_,_,_,_,_,_,_),(_,_,_,_)) = j
  massign _ v ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n),(o,p,q,r,s,t,u),(w,x,y,z)) = ((a,b,c,d,e,f,g),(h,i,Just v,k,l,m,n),(o,p,q,r,s,t,u),(w,x,y,z))

instance Memo Att_LEN MemoTable Int where
  mlookup _   ((_,_,_,_,_,_,_),(_,_,_,k,_,_,_),(_,_,_,_,_,_,_),(_,_,_,_)) = k
  massign _ v ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n),(o,p,q,r,s,t,u),(w,x,y,z)) = ((a,b,c,d,e,f,g),(h,i,j,Just v,l,m,n),(o,p,q,r,s,t,u),(w,x,y,z))

instance Memo Att_FMTSFL MemoTable Formats where
  mlookup _   ((_,_,_,_,_,_,_),(_,_,_,_,l,_,_),(_,_,_,_,_,_,_),(_,_,_,_)) = l
  massign _ v ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n),(o,p,q,r,s,t,u),(w,x,y,z)) = ((a,b,c,d,e,f,g),(h,i,j,k,Just v,m,n),(o,p,q,r,s,t,u),(w,x,y,z))

instance Memo Att_FMTSI MemoTable Formats where
  mlookup _   ((_,_,_,_,_,_,_),(_,_,_,_,_,m,_),(_,_,_,_,_,_,_),(_,_,_,_)) = m
  massign _ v ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n),(o,p,q,r,s,t,u),(w,x,y,z)) = ((a,b,c,d,e,f,g),(h,i,j,k,l,Just v,n),(o,p,q,r,s,t,u),(w,x,y,z))

instance Memo Att_PW MemoTable T_PW where
  mlookup _   ((_,_,_,_,_,_,_),(_,_,_,_,_,_,n),(_,_,_,_,_,_,_),(_,_,_,_)) = n
  massign _ v ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n),(o,p,q,r,s,t,u),(w,x,y,z)) = ((a,b,c,d,e,f,g),(h,i,j,k,l,m,Just v),(o,p,q,r,s,t,u),(w,x,y,z))

instance Memo Att_MINLLI MemoTable T_PW where
  mlookup _   ((_,_,_,_,_,_,_),(_,_,_,_,_,_,_),(o,_,_,_,_,_,_),(_,_,_,_)) = o
  massign _ v ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n),(o,p,q,r,s,t,u),(w,x,y,z)) = ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n),(Just v,p,q,r,s,t,u),(w,x,y,z))

instance Memo Att_MINWI MemoTable T_PW where
  mlookup _   ((_,_,_,_,_,_,_),(_,_,_,_,_,_,_),(_,p,_,_,_,_,_),(_,_,_,_)) = p
  massign _ v ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n),(o,p,q,r,s,t,u),(w,x,y,z)) = ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n),(o,Just v,q,r,s,t,u),(w,x,y,z))

instance Memo Att_MAXHI MemoTable T_PH where
  mlookup _   ((_,_,_,_,_,_,_),(_,_,_,_,_,_,_),(_,_,q,_,_,_,_),(_,_,_,_)) = q
  massign _ v ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n),(o,p,q,r,s,t,u),(w,x,y,z)) = ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n),(o,p,Just v,r,s,t,u),(w,x,y,z))

instance Memo Att_FILLERRS MemoTable T_ERRS where
  mlookup _   ((_,_,_,_,_,_,_),(_,_,_,_,_,_,_),(_,_,_,r,_,_,_),(_,_,_,_)) = r
  massign _ v ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n),(o,p,q,r,s,t,u),(w,x,y,z)) = ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n),(o,p,q,Just v,s,t,u),(w,x,y,z))

instance Memo Att_IFILLERRS MemoTable T_ERRS where
  mlookup _   ((_,_,_,_,_,_,_),(_,_,_,_,_,_,_),(_,_,_,_,s,_,_),(_,_,_,_)) = s
  massign _ v ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n),(o,p,q,r,s,t,u),(w,x,y,z)) = ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n),(o,p,q,r,Just v,t,u),(w,x,y,z))

instance Memo Att_ERRORL MemoTable T_ERRS where
  mlookup _   ((_,_,_,_,_,_,_),(_,_,_,_,_,_,_),(_,_,_,_,_,t,_),(_,_,_,_)) = t
  massign _ v ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n),(o,p,q,r,s,t,u),(w,x,y,z)) = ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n),(o,p,q,r,s,Just v,u),(w,x,y,z))

instance Memo Att_ERRORI MemoTable T_ERROR where
  mlookup _   ((_,_,_,_,_,_,_),(_,_,_,_,_,_,_),(_,_,_,_,_,_,u),(_,_,_,_)) = u
  massign _ v ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n),(o,p,q,r,s,t,u),(w,x,y,z)) = ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n),(o,p,q,r,s,t,Just v),(w,x,y,z))

instance Memo Att_REQS MemoTable T_REQS where
  mlookup _   ((_,_,_,_,_,_,_),(_,_,_,_,_,_,_),(_,_,_,_,_,_,_),(w,_,_,_)) = w
  massign _ v ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n),(o,p,q,r,s,t,u),(w,x,y,z)) = ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n),(o,p,q,r,s,t,u),(Just v,x,y,z))

instance Memo Att_IREQS MemoTable T_REQS where
  mlookup _   ((_,_,_,_,_,_,_),(_,_,_,_,_,_,_),(_,_,_,_,_,_,_),(_,x,_,_)) = x
  massign _ v ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n),(o,p,q,r,s,t,u),(w,x,y,z)) = ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n),(o,p,q,r,s,t,u),(w,Just v,y,z))

instance Memo Att_FILLMINS MemoTable T_MINS where
  mlookup _   ((_,_,_,_,_,_,_),(_,_,_,_,_,_,_),(_,_,_,_,_,_,_),(_,_,y,_)) = y
  massign _ v ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n),(o,p,q,r,s,t,u),(w,x,y,z)) = ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n),(o,p,q,r,s,t,u),(w,x,Just v,z))

instance Memo Att_MINS MemoTable T_MINS where
  mlookup _   ((_,_,_,_,_,_,_),(_,_,_,_,_,_,_),(_,_,_,_,_,_,_),(_,_,_,z)) = z
  massign _ v ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n),(o,p,q,r,s,t,u),(w,x,y,z)) = ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n),(o,p,q,r,s,t,u),(w,x,y,Just v))

instance Memoizable PPS MemoTable where 
  updMemoTable = updMemoTable'
  getMemoTable = getMemoTable'

instance StrategicData (PPS MemoTable) where 
  isTerminal z = isJust (getHole z :: Maybe MemoTable)
              -- || isJust (getHole z :: Maybe Name     )
              -- || isJust (getHole z :: Maybe Int      )