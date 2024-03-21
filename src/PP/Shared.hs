{-# LANGUAGE DeriveDataTypeable #-}

module Examples.AG.PP.Shared where


import Data.Data


data PPRoot = Best PPS
            | All PPS
            | Displ PPS
            deriving (Typeable, Data, Show)

data PPS = Empty
         | Text String
         | Indent Int PPS
         | Beside PPS PPS
         | Above PPS PPS
         | Dup PPS PPS
         | Join PPS
         --------------------
         | Apply PPC PPSArgs
         --------------------
         | Filla FillList
         | FillBlock Int FillList
        deriving (Typeable, Data, Show)


data PPC = ParC
         | IndentC Int PPC
         | BesideC PPC PPC
         | AboveC PPC PPC
         | DupC PPC PPC
         | JoinC PPC
         -------------------
         | ApplyC PPC PPCArgs
         -------------------
        deriving (Typeable, Data, Show)


data PPSArgs = ConsArgs PPSArgs PPS
             | NilArgs
            deriving (Typeable, Data, Show)


data PPCArgs = ConsPPCArgs PPCArgs PPC
             | NilPPCArgs
            deriving (Typeable, Data, Show)


data FillList = ConsFillList FillList PPS
              | NilFillList
              deriving (Typeable, Data, Show)

------------------------------------------------------------

type T_PW  = Int
type T_PLL = Int
type T_PH  = Int

type T_MINS = [Sizes]

type Sizes = (T_PW, T_PLL, T_PH)

data T_FRAME = F  T_PW   T_PLL
             deriving (Eq, Show)

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

type T_FMTS = [T_FORMATS]


type T_ERROR = Bool

type T_ERRS = [T_ERROR]
