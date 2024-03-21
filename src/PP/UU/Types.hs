module Examples.AG.PP.UU.Types where


type Formats = [Format]

{- Pretty-printer combinators with global page width -}

type T_PW  = Int
type T_PLL = Int
type T_PH  = Int
--                Width  Width last line
data T_Frame = F  T_PW   T_PLL
             deriving Eq

instance Ord T_Frame where
  (F w _) <= (F w' _) = w <= w'
  max x@(F w _) y@(F w' _)
    | w > w'    = x
    | otherwise = y


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

type T_Mins  = [ (T_PW, T_PLL, T_PH) ]

type T_Reqs  = [ T_Frame ]

type T_Fmts = [ T_Formats ]
type T_Errs = [ T_Error ]

type T_Error = Bool

data T_Formats = AFormat   Formats
               | TFormats  Formats  Formats  T_Error  T_Error

type T_Function = T_Formats -> T_Formats

type T_SynPPS = ( T_Formats, T_Error, T_PH, T_PLL, T_PW )