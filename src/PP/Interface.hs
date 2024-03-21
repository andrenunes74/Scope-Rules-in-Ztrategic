module Examples.AG.PP.Interface where


import Data.Generics.Zipper
import Examples.AG.PP.Shared
import Examples.AG.PP.Pretty




infixr 3 >|< , >>|<<         -- beside
infixr 2 >-< , >>-<<         -- above
infixr 1 >//<, >>//<<        -- 
infixr 0 >>$<, >>$<<         -- apply

-- -------------------------------------------------------------------
-- PP class ----------------------------------------------------------

type PP_Doc = PPS
type PP_Exp = PPC

class Show a => PP a where
  pp    :: a -> PP_Doc
  pp       = text . show

  ppList :: [a] -> PP_Doc
  ppList as = if null as
              then empty
              else foldr ((>|<) . pp) empty as

instance PP PPS where
  pp     = id

instance PP Char where
  pp c   = text [c]
  ppList = text


instance PP a => PP [a] where
  pp = ppList


-- -------------------------------------------------------------------
-- Single layout -----------------------------------------------------

empty :: PP_Doc
empty = Empty

notext :: PP_Doc
notext = text ""

text :: String -> PP_Doc
text = Text

indent :: Int -> PP_Doc -> PP_Doc
indent = Indent

-- l beside r 
(>|<) :: (PP a, PP b) => a -> b -> PP_Doc
l >|< r  = Beside ppl ppr
  where ppl = pp l
        ppr = pp r

-- u above l 
(>-<) :: (PP a, PP b) => a -> b -> PP_Doc
u >-< l  = Above ppu ppl
  where ppu = pp u
        ppl = pp l

-- l beside r, with a space inbetween
(>#<) :: (PP b, PP a) => a -> b -> PP_Doc
l >#< r = l >|< text " " >|< r



fill :: PP a => [a] -> PP_Doc
fill = Filla . foldr (flip ConsFillList . pp) NilFillList

-- join all elements of list into a single PP_Doc, with maximum width of `i`, continuing on the next line when width is reached
fillblock :: PP a => Int -> [a] -> PP_Doc
fillblock i = FillBlock i . foldr (flip ConsFillList . pp) NilFillList

-- -------------------------------------------------------------------
-- Multiple layout combinators ---------------------------------------

(>//<) :: (PP a, PP b) => a -> b -> PP_Doc
a  >//<  b  = Dup  ppa ppb
  where ppa = pp a
        ppb = pp b

join :: PP_Doc -> PP_Doc
join = Join

cindent :: Int -> PP_Exp -> PP_Exp
cindent = IndentC

(>>|<<), (>>-<<), (>>//<<) :: PP_Exp -> PP_Exp -> PP_Exp
l  >>|<< r  =  BesideC l r
u  >>-<< l  =  AboveC u l
a >>//<< b  =  DupC a b

cjoin :: PP_Exp -> PP_Exp
cjoin = JoinC

-- generate a layout hole to be filled
par :: PP_Exp
par = ParC

-- apply layout `e` to list of PP_Docs `pl`, sticking each element of `pl` into a hole of `e`
(>>$<) :: PP a => PP_Exp -> [a] -> PP_Doc
e >>$< pl = Apply e . foldr (flip ConsArgs . pp) NilArgs $ pl

-- sequentially stick each layout in `pl` into a hole in `e`
(>>$<<) :: PP_Exp -> [PP_Exp] -> PP_Exp
e >>$<< pl = ApplyC e . foldr (flip ConsPPCArgs) NilPPCArgs $ pl


-- Join two layouts, such that the second one is only used if the first one does not fit
(>>^<<) :: PP_Exp -> PP_Exp -> PP_Exp
a >>^<< b  =  cjoin (a >>//<< b)


hv2i :: (PP a, PP b) => a -> b -> PP_Doc
hv2i a b = join (a >|< b >//< a >-< b)


-- -------------------------------------------------------------------
-- Displaying the result ---------------------------------------------

render :: PP_Doc -> Int -> IO ()
render p f = putStr $ fmtsRoot (toZipper (Best p)) f

renderAll :: PP_Doc -> Int -> IO ()
renderAll p f = putStr $ fmtsRoot (toZipper (All p)) f

disp :: PP_Doc -> Int -> String
disp p = fmtsRoot (toZipper (Displ p))
