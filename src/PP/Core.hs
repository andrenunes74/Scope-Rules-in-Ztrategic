module Examples.AG.PP.Core where

import Examples.AG.PP.Shared

-- ...................................................................
-- ..... Basic machinery .............................................

emptyFmts ::Formats
emptyFmts = []

textFmts :: String -> Formats
textFmts s = [ s2fmt s ]

indentFmts :: T_FRAME -> Int -> Formats -> Formats
indentFmts (F pw _) i = map (indentFmt i)
                        . dropWhile (notFits (pw - i))

notFits :: T_PW -> Format -> Bool
notFits delta e = total_w e > delta

besideFmts :: T_FRAME -> Formats -> Formats -> Formats
besideFmts (F pw _) left  right
  = mergel [ map (l `besideFmt`)
           . dropWhile (tooWide pw l)
           $ right
           | l <- left
           ]

tooWide :: T_PW -> Format -> Format -> Bool
tooWide pw x y
  = (total_w x `max` (last_w x + total_w y)) > pw

aboveFmts :: Formats -> Formats -> Formats
aboveFmts [] _ = []
aboveFmts _ [] = []
aboveFmts up@(upper:ru) low@(lower:rl)
   | utw >= ltw = firstelem : aboveFmts ru low
   | otherwise = firstelem : aboveFmts up rl
   where utw = total_w upper
         ltw = total_w lower
         firstelem = upper `aboveFmt` lower


{- Pretty-printing with error correction -}

errorIndent :: Int -> Formats -> Formats
errorIndent i = map (indentFmt i)

errorBeside :: Formats -> Formats -> Formats
errorBeside left right = mergel [ map (l `besideFmt`) right
                                 | l <- left
                                 ]

-- -------------------------------------------------------------------
-- Formatting one layout ---------------------------------------------


s2fmt     :: String -> Format
s2fmt s   = Elem 1 l l (const (s ++))
  where l = length s

indentFmt :: Int -> Format -> Format
indentFmt i   (Elem dh dl dw dt)
   = Elem dh (i + dl) (i + dw) (\n -> (sp i ++) . dt (i + n))

aboveFmt, besideFmt :: Format -> Format -> Format
(Elem uh _ uw ut) `aboveFmt` (Elem lh ll lw lt)
  = Elem (uh + lh) ll (uw `max` lw)
         (make_ts_above ut lt)
  where make_ts_above u l   = \n -> let nl_skip = (('\n':sp n)++)
                                    in  u n . nl_skip . l n
(Elem lh ll lw lt) `besideFmt` (Elem rh rl rw rt)
  = Elem (lh + rh - 1) (ll + rl)
         (lw `max` (ll + rw)) (\n -> lt n . rt (ll + n))

-- -------------------------------------------------------------------
-- Display the layout found ------------------------------------------

best :: [Format] -> String
best fs  = if null fs then "" else (txtstr . head $ fs) 0 ""
allf :: [Format] -> String
allf     = concatMap (\fmt -> txtstr fmt 0 "\n\n")
dispf :: [Format] -> String
dispf fs = if null fs then "" else (txtstr . head $ fs) 0 ""

-- -------------------------------------------------------------------
-- Utility functions -------------------------------------------------

merge :: Ord a => [a] -> [a] -> [a]
merge []        ys        = ys
merge xs        []        = xs
merge xl@(x:xs) yl@(y:ys)
  | x == y    = x : merge xs ys
  | x <  y    = x : merge xs yl
  | otherwise = y : merge xl ys

spaces :: String
spaces = ' ':spaces

sp :: Int -> String 
sp n = if n >= 0 then take n spaces else ""

mergel :: Ord a => [[a]] -> [a]
mergel = foldr merge []
