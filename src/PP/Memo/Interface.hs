{-# LANGUAGE FlexibleContexts,
             FlexibleInstances
#-}
module PP.Memo.Interface where


import Data.Generics.Zipper
import PP.Memo.SharedMemo
import PP.Memo.Pretty




infixr 3 >|< , >>|<<         -- beside
infixr 2 >-< , >>-<<         -- above
infixr 1 >//<, >>//<<        -- 
infixr 0 >>$<, >>$<<         -- apply

-- -------------------------------------------------------------------
-- PP class ----------------------------------------------------------

type PP_Doc = PPS MemoTable

class Show a => PP a where
  pp    :: a -> PP_Doc
  pp       = text . show

  ppList :: [a] -> PP_Doc
  ppList as = if null as
              then empty
              else foldr ((>|<) . pp) empty as

instance PP PP_Doc where
  pp     = id

instance PP Char where
  pp c   = text [c]
  ppList = text


instance PP a => PP [a] where
  pp = ppList


-- -------------------------------------------------------------------
-- Single layout -----------------------------------------------------

empty :: PP_Doc
empty = Empty emptyMemo

notext :: PP_Doc
notext = text ""

text :: String -> PP_Doc
text x = Text x emptyMemo

indent :: Int -> PP_Doc -> PP_Doc
indent i x = Indent i x emptyMemo

(>|<) :: (PP a, PP b) => a -> b -> PP_Doc
l >|< r  = Beside ppl ppr emptyMemo
  where ppl = pp l
        ppr = pp r

(>-<) :: (PP a, PP b) => a -> b -> PP_Doc
u >-< l  = Above ppu ppl emptyMemo
  where ppu = pp u
        ppl = pp l


(>#<) :: (PP b, PP a) => a -> b -> PP_Doc
l >#< r = l >|< text " " >|< r



fill :: PP a => [a] -> PP_Doc
fill l = Filla (foldr ((\a b -> ConsFillList b a emptyMemo) . pp) (NilFillList emptyMemo) l) emptyMemo


fillblock :: PP a => Int -> [a] -> PP_Doc
fillblock i l = FillBlock i (foldr ((\a b -> ConsFillList b a emptyMemo) . pp) (NilFillList emptyMemo) l) emptyMemo

-- -------------------------------------------------------------------
-- Multiple layout combinators ---------------------------------------

(>//<) :: (PP a, PP b) => a -> b -> PP_Doc
a  >//<  b  = Dup  ppa ppb emptyMemo
  where ppa = pp a
        ppb = pp b

join :: PP_Doc -> PP_Doc
join p = Join p emptyMemo

cindent :: Int -> PP_Doc -> PP_Doc
cindent i p = IndentC i p emptyMemo

(>>|<<), (>>-<<), (>>//<<) :: PP_Doc -> PP_Doc -> PP_Doc
l  >>|<< r  =  BesideC l r emptyMemo
u  >>-<< l  =  AboveC u l emptyMemo
a >>//<< b  =  DupC a b emptyMemo

cjoin :: PP_Doc -> PP_Doc
cjoin p = JoinC p emptyMemo

par :: PP_Doc
par = ParC emptyMemo


(>>$<) :: PP a => PP_Doc -> [a] -> PP_Doc
e >>$< pl = Apply e (foldr ((\a b -> ConsArgs b a emptyMemo) . pp) (NilArgs emptyMemo) $ pl) emptyMemo


(>>$<<) :: PP_Doc -> [PP_Doc] -> PP_Doc
e >>$<< pl = ApplyC e (foldr ((\a b -> ConsPPCArgs b a emptyMemo)) (NilPPCArgs emptyMemo) $ pl) emptyMemo

(>>^<<) :: PP_Doc -> PP_Doc -> PP_Doc
a >>^<< b  =  cjoin (a >>//<< b)

hv2i :: (PP a, PP b) => a -> b -> PP_Doc
hv2i a b = join (a >|< b >//< a >-< b)


-- -------------------------------------------------------------------
-- Displaying the result ---------------------------------------------

render :: PP_Doc -> Int -> IO ()
render p f = putStr $ fmtsRoot (toZipper (Best p emptyMemo)) f

renderAll :: PP_Doc -> Int -> IO ()
renderAll p f = putStr $ fmtsRoot (toZipper (All p emptyMemo)) f

disp :: PP_Doc -> Int -> String
disp p = fmtsRoot (toZipper (Displ p emptyMemo))
