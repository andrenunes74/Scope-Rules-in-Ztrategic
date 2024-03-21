module Examples.AG.PP.LRC.Types where

import Examples.AG.PP.LRC.LrcPrelude

-- type Formats = [Format]

{- Pretty-printer combinators with global page width -}

type T_PW  = Integer
type T_PLL = Integer
type T_PH  = Integer

data Sizes
        = C_Triple_1 Integer Integer Integer
        deriving (Eq , Ord)

--                Width  Width last line
data T_Frame = C_F_1  T_PW   T_PLL
             deriving Eq

instance Ord T_Frame where
  max x@(C_F_1 w _) y@(C_F_1 w' _)
    | w > w'    = x
    | otherwise = y


data Format = Elem { height  :: T_PH
                   , last_w  :: T_PLL
                   , total_w :: T_PW
                   , txtstr  :: Integer -> String -> String
                   }

instance Eq Format  where
  x == y =  height x  == height y
         && total_w x == total_w y
         && last_w  x == last_w  y

instance Ord Format where
  x <  y =  height x < height y
         || (  height x == height y 
            && total_w x < total_w y )



-- type T_PPS =  T_Frame -> (T_Formats,T_Error,T_PH,T_PLL,T_PW)

type T_PPS_2 = ( T_Frame -> (T_Error,T_Formats) )

type T_PPS =  ( T_PPS_2 , T_PH , T_PLL , T_PW )


-- type T_PPC =  T_Errs -> T_Fmts -> T_Frame -> T_Mins ->
--               (T_Formats,T_Error,T_PH,T_Reqs,T_PLL
--               ,T_PW,Int
--               ) 
   
type T_PPC_4 = ( T_Errs -> T_Fmts -> (T_Error , T_Formats) )

type T_PPC_3 = ( T_Frame -> (T_PPC_4 , T_Reqs) )

type T_PPC_2 = ( T_Mins -> (T_PPC_3 , T_PH , T_PLL , T_PW ) )

type T_PPC   = ( T_PPC_2 , Integer )


--
-- Type of Attributes
--

type Formats = [Format]
data Pair_Formats
        = C_C_Pair_Formats_1 Formats BOOL
        deriving (Eq , Ord)
data Pair_T_Formats
        = C_C_Pair_T_Formats_1 T_Formats BOOL
        deriving (Eq , Ord)

type T_Errs = [BOOL]
type T_Fmts = [T_Formats]
data T_Formats
        = C_AFormat_1 Formats
        | C_TFormats_1 Formats Formats BOOL BOOL
        deriving (Eq , Ord)

type T_Mins = [Sizes]
type T_Reqs = [T_Frame]


type T_Error = Bool