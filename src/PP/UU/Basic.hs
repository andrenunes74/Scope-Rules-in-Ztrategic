--  $Header: /data/cvs-rep/uust/lib/pretty/UU/Pretty/Basic.hs,v 1.2 2003/02/26 11:18:27 uust Exp $
--  $Name:  $ (version name)

module Examples.AG.PP.UU.Basic ( PP (..), PP_Doc, PP_Exp
                   -- Single layout combinators
                 , empty, text, indent, (>|<), (>-<), (>#<), fill , fillblock
                   -- Multiple layout combinators
                 , (>//<), join, par, (>>$<), (>>^<<)
                 , eindent, (>>|<<), (>>-<<), (>>//<<), ejoin, (>>$<<), hv2i
                   -- Displaying the result
                 , render, renderAll, disp
                   -- Additional generated combinators
                 , c2e, element_h1, eelement_h1, vcenter, invisible
                   -- Additional derived combinators
                 , fpar, spar
                 ) where

{- Pretty-printers and pretty-printing combinators. Version 2.0d
   Authors: S. Doaitse Swierstra and Pablo R. Azero
   Date: July, 1999
 -}


import Examples.AG.PP.UU.Types
import Examples.AG.PP.UU.Evaluator

-- ...................................................................
-- ..... Interface definition ........................................

infixr 3 >|< , >>|<<
infixr 2 >-< , >>-<<
infixr 1 >//<, >>//<<
infixr 0 >>$<, >>$<<

-- -------------------------------------------------------------------
-- PP class ----------------------------------------------------------

newtype PP_Doc = PPDoc T_PPS

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
  show p = disp p 200 ""

-- -------------------------------------------------------------------
-- Single layout combinators -----------------------------------------

empty :: PP_Doc
empty = PPDoc sem_PPS_Empty

text :: String -> PP_Doc
text  = PPDoc . sem_PPS_Text

indent :: PP a => Int -> a -> PP_Doc
indent i fs = PPDoc (sem_PPS_Indent i nfs)
   where (PPDoc nfs) = pp fs

(>|<) :: (PP a, PP b) => a -> b -> PP_Doc
l >|< r  = PPDoc (sem_PPS_Beside ppl ppr)
  where (PPDoc ppl) = pp l
        (PPDoc ppr) = pp r

(>-<) :: (PP a, PP b) => a -> b -> PP_Doc
u >-< l  = PPDoc (sem_PPS_Above ppu ppl)
  where (PPDoc ppu) = pp u
        (PPDoc ppl) = pp l

(>#<) :: (PP b, PP a) => a -> b -> PP_Doc
l >#< r = l >|< text " " >|< r

fill :: PP a => [a] -> PP_Doc
fill = PPDoc . sem_PPS_Fill . foldr fill_alg sem_FillList_Nil
  where fill_alg f
          = sem_FillList_Cons (case (pp f) of (PPDoc ppp) -> ppp)

fillblock :: PP a => Int -> [a] -> PP_Doc
fillblock i = PPDoc . sem_PPS_FillBlock i . foldr fill_alg sem_FillList_Nil
  where fill_alg f
          = sem_FillList_Cons (case (pp f) of (PPDoc ppp) -> ppp)

-- -------------------------------------------------------------------
-- Multiple layout combinators ---------------------------------------

(>//<) :: (PP a, PP b) => a -> b -> PP_Doc
a  >//<  b  = PPDoc (sem_PPS_Dup  ppa ppb)
  where (PPDoc ppa) = pp a
        (PPDoc ppb) = pp b

join :: PP_Doc -> PP_Doc
join (PPDoc d) = PPDoc . sem_PPS_Join $ d

newtype PP_Exp = PPExp T_PPC

eindent :: Int -> PP_Exp -> PP_Exp
eindent i (PPExp ppc) = PPExp (sem_PPC_Indent i ppc)

(>>|<<), (>>-<<), (>>//<<) :: PP_Exp -> PP_Exp -> PP_Exp
(PPExp l)  >>|<< (PPExp r)  =  PPExp (sem_PPC_Beside l r)
(PPExp u)  >>-<< (PPExp l)  =  PPExp (sem_PPC_Above  u l)
(PPExp a) >>//<< (PPExp b)  =  PPExp (sem_PPC_Dup    a b)

ejoin :: PP_Exp -> PP_Exp
ejoin (PPExp dc) = PPExp . sem_PPC_Join $ dc

par :: PP_Exp
par = PPExp sem_PPC_Par

(>>$<) :: PP a => PP_Exp -> [a] -> PP_Doc
(PPExp e) >>$< pl = PPDoc . sem_PPS_Apply e . foldr ppslist sem_PPSArgs_Nil $ pl
  where ppslist p = sem_PPSArgs_Cons (case (pp p) of (PPDoc ppp) -> ppp)

(>>$<<) :: PP_Exp -> [PP_Exp] -> PP_Exp
(PPExp e) >>$<< pl = PPExp . sem_PPC_Apply e . foldr ppclist sem_PPCArgs_Nil $ pl
  where ppclist (PPExp p) = sem_PPCArgs_Cons p

(>>^<<) :: PP_Exp -> PP_Exp -> PP_Exp
a >>^<< b  =  ejoin (a >>//<< b)

hv2i a b = join (a >|< b >//< a >-< b)

-- -------------------------------------------------------------------
-- Displaying the result ---------------------------------------------

render, renderAll   ::  PP_Doc -> Int -> IO ()
render    (PPDoc fs)  =  putStr . sem_Root_Best fs
renderAll (PPDoc fs)  =  putStr . sem_Root_All fs

disp  ::  PP_Doc -> Int -> ShowS
disp (PPDoc fs) =  sem_Disp_Disp fs

-- -------------------------------------------------------------------
-- Additional generated combinators ----------------------------------

c2e :: PP a => a -> PP_Exp
c2e s = let (PPDoc s') = pp s in PPExp . sem_PPC_Pps $ s'

element_h1 :: PP_Doc -> PP_Doc
element_h1 = \(PPDoc fs) -> PPDoc (sem_PPS_Filt fs)

eelement_h1 :: PP_Exp -> PP_Exp
eelement_h1 (PPExp pe) = PPExp . sem_PPC_Filt $ pe

vcenter :: PP a => [ a ] -> PP_Doc
vcenter = PPDoc . sem_PPS_Center . foldr center_alg sem_CenterList_Nil
  where center_alg f = sem_CenterList_Cons (case (pp f) of (PPDoc pf) -> pf)

invisible :: PP_Doc -> PP_Doc
invisible (PPDoc a) = PPDoc . sem_PPS_Inv $ a

-- -------------------------------------------------------------------
-- Additional derived combinators ------------------------------------

fpar, spar :: PP_Exp
fpar = plift  first   par
spar = plift  second  par

first fs  = case fs of
              (TFormats fa _ ea _) -> (AFormat fa, ea   )
              (AFormat fa)         -> (AFormat fa, False)
second fs = case fs of
              (TFormats _ fb _ eb) -> (AFormat fb, eb   )
              (AFormat fb)         -> (AFormat fb, False)

-- Utilities

lift :: (T_Formats -> T_Formats) -> PP_Doc -> PP_Doc
lift f (PPDoc p) = PPDoc . sem_LiftS_Lift p $ f

--elift :: (T_Formats -> T_Formats) -> T_PPC -> T_PPC
elift f (PPExp e) = PPExp . sem_LiftC_Lift e $ f

--plift :: (a -> b) -> T_PPC -> T_PPC
plift f (PPExp e) = PPExp . sem_LiftC_Pair e $ f

