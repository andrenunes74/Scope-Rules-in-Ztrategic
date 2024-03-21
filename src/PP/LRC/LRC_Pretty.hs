module Examples.AG.PP.LRC.LRC_Pretty where

-- *******************************************************************
-- Interface.ag: interface definition *******************************

import Examples.AG.PP.LRC.LrcPrelude
import Examples.AG.PP.LRC.Core
import Examples.AG.PP.LRC.Types
import Examples.AG.PP.LRC.Evaluator

-- *******************************************************************
-- Interface.ag: interface definition *******************************

infixr 3 >|< , >>|<<         -- beside
infixr 2 >-< , >>-<<         -- above
infixr 1 >//<, >>//<<        -- 
infixr 0 >>$<, >>$<<         -- apply


-- -------------------------------------------------------------------
-- PP class ----------------------------------------------------------

newtype PP_Doc = PP_Doc T_PPS 

class Show a => PP a where
  pp     :: a   -> PP_Doc
  pp       = text . show

  ppList :: [a] -> PP_Doc
  ppList as = if null as
              then empty
              else foldr (>|<) empty . map pp $ as
  
instance PP PP_Doc where
  pp     = id

instance PP Char where
  pp c   = text [c]
  ppList = text

instance PP a => PP [a] where
  pp = ppList

instance Show PP_Doc where
  show x = undefined


-- -------------------------------------------------------------------
-- Single layout combinators -----------------------------------------

empty :: PP_Doc
empty = PP_Doc sem_PPS_Empty

text :: String -> PP_Doc
text  = PP_Doc . sem_PPS_Text

indent :: PP a => Integer -> a -> PP_Doc
indent i fs = PP_Doc (sem_PPS_Indent i nfs)
   where (PP_Doc nfs) = pp fs

(>|<) :: (PP a, PP b) => a -> b -> PP_Doc
l >|< r  = PP_Doc (sem_PPS_Beside ppl ppr)
  where (PP_Doc ppl) = pp l
        (PP_Doc ppr) = pp r

(>-<) :: (PP a, PP b) => a -> b -> PP_Doc
u >-< l  = PP_Doc (sem_PPS_Above ppu ppl)
  where (PP_Doc ppu) = pp u
        (PP_Doc ppl) = pp l

(>#<) :: (PP b, PP a) => a -> b -> PP_Doc
l >#< r = l >|< text " " >|< r


fill :: PP a => [a] -> PP_Doc
fill = PP_Doc . sem_PPS_Fill . foldr fill_alg sem_FillList_Nil
  where fill_alg f
          = sem_FillList_Cons (case (pp f) of (PP_Doc ppp) -> ppp)

fillblock :: PP a => Integer -> [a] -> PP_Doc
fillblock i = PP_Doc . sem_PPS_FillBlock i . foldr fill_alg sem_FillList_Nil
  where fill_alg f
          = sem_FillList_Cons (case (pp f) of (PP_Doc ppp) -> ppp)

-- -------------------------------------------------------------------
-- Multiple layout combinators ---------------------------------------

(>//<) :: (PP a, PP b) => a -> b -> PP_Doc
a  >//<  b  = PP_Doc (sem_PPS_Dup  ppa ppb)
  where (PP_Doc ppa) = pp a
        (PP_Doc ppb) = pp b

join :: PP_Doc -> PP_Doc
join (PP_Doc d) = PP_Doc . sem_PPS_Join $ d

type PP_Exp = T_PPC

cindent :: Integer -> PP_Exp -> PP_Exp
cindent = sem_PPC_Indent

(>>|<<), (>>-<<), (>>//<<) :: PP_Exp -> PP_Exp -> PP_Exp
l  >>|<< r  =  sem_PPC_Beside l r
u  >>-<< l  =  sem_PPC_Above u l
a >>//<< b  =  sem_PPC_Dup a b

cjoin :: PP_Exp -> PP_Exp
cjoin dc = sem_PPC_Join dc

par :: PP_Exp
par = sem_PPC_Par

(>>$<) :: PP a => PP_Exp -> [a] -> PP_Doc
e >>$< pl = PP_Doc . sem_PPS_Apply e . foldr ppslist sem_PPSArgs_Nil $ pl
  where ppslist p = sem_PPSArgs_Cons (case (pp p) of (PP_Doc ppp) -> ppp)

(>>$<<) :: PP_Exp -> [PP_Exp] -> PP_Exp
e >>$<< pl = sem_PPC_Apply e . foldr sem_PPCArgs_Cons sem_PPCArgs_Nil $ pl

(>>^<<) :: PP_Exp -> PP_Exp -> PP_Exp
a >>^<< b  =  cjoin (a >>//<< b)

hv2i a b = join (a >|< b >//< a >-< b)

-- -------------------------------------------------------------------
-- Displaying the result ---------------------------------------------

render, renderAll   ::  PP_Doc -> Integer -> IO ()
render    (PP_Doc fs)  =  putStr . sem_Root_Best fs
renderAll (PP_Doc fs)  =  putStr . sem_Root_All fs

disp  ::  PP_Doc -> Integer -> ShowS
disp (PP_Doc fs) =  sem_Disp_Disp fs







