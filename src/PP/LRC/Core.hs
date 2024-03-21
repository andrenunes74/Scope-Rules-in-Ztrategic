module Examples.AG.PP.LRC.Core where

import Examples.AG.PP.LRC.LrcPrelude
import Examples.AG.PP.LRC.Types

-- *******************************************************************
-- Core.ag: basic machine ********************************************

empty_fmts ::Formats
empty_fmts = []

text_fmts :: String -> Formats
text_fmts s = [ s2fmt s ]

indent_fmts :: T_Frame -> Integer -> Formats -> Formats
indent_fmts (C_F_1 pw _) i = map (indent_fmt i)
                       . dropWhile (notFits (pw - i))       
notFits delta e = total_w e > delta

beside_fmts :: T_Frame -> Formats -> Formats -> Formats
beside_fmts (C_F_1 pw _) left  right
  = mergel [ map (l `beside_fmt`)
           . dropWhile (tooWide pw l)
           $ right
           | l <- left
           ]
tooWide pw x y
  = (total_w x `max` (last_w x + total_w y)) > pw

above_fmts :: Formats -> Formats -> Formats
above_fmts [] ls = []
above_fmts us [] = []
above_fmts up@(upper:ru) low@(lower:rl)
  | utw >= ltw = firstelem : above_fmts ru low
  | utw <  ltw = firstelem : above_fmts up rl
  where utw = total_w upper
        ltw = total_w lower
        firstelem = upper `above_fmt` lower

{- Pretty-printing with error correction -}

error_indent :: Integer -> Formats -> Formats
error_indent i = map (indent_fmt i)

error_beside :: Formats -> Formats -> Formats
error_beside left right = mergel [ map (l `beside_fmt`) right
                                 | l <- left
                                 ]

-- -------------------------------------------------------------------
-- Formatting one layout ---------------------------------------------


s2fmt     :: String -> Format
s2fmt s   = Elem 1 l l (\_ -> (s++))
  where l = toInteger(length s)

indent_fmt :: Integer -> Format -> Format
indent_fmt i   (Elem dh dl dw dt)
   = Elem dh (i + dl) (i + dw) (\n -> ((sp i) ++) . dt (i + n))

above_fmt, beside_fmt :: Format -> Format -> Format
(Elem uh ul uw ut) `above_fmt` (Elem lh ll lw lt)
  = Elem (uh + lh) ll (uw `max` lw)
         (make_ts_above ut lt)
  where make_ts_above ut lt = \n -> let nl_skip = (('\n':sp n)++)
                                    in  ut n . nl_skip . lt n
(Elem lh ll lw lt) `beside_fmt` (Elem rh rl rw rt)
  = Elem (lh + rh - 1) (ll + rl)
         (lw `max` (ll + rw)) (\n -> lt n . rt (ll + n))

-- -------------------------------------------------------------------
-- Display the layout found ------------------------------------------

best fs  = if null fs then "" else (txtstr . head $ fs) 0 ""
allf :: [Format] -> String
allf     = concatMap (\fmt -> (txtstr fmt) 0 "\n\n")
dispf fs = if null fs then id else (txtstr . head $ fs) 0

-- -------------------------------------------------------------------
-- Utility functions -------------------------------------------------

merge []        ys        = ys
merge xs        []        = xs
merge xl@(x:xs) yl@(y:ys)
  | x == y    = x : merge xs ys
  | x <  y    = x : merge xs yl
  | otherwise = y : merge xl ys


spaces = ' ':spaces

sp :: Integer -> String
sp n = take (toInt n) spaces

mergel :: Ord a => [[a]] -> [a]
mergel = foldr merge []
