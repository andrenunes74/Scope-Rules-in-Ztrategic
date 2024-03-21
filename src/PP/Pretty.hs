module PP.Pretty where


import Data.Generics.Zipper
import Language.Grammars.ZipperAG hiding ((.|))
import PP.Shared
import PP.SharedAG
import PP.Core




minw :: Zipper PPRoot -> T_PW
minw t = case constructor t of
  CEmpty -> 0
  CText -> length (lexemeT t)
  CIndent -> lexemeI t + minw(t.$2)
  CBeside -> max (minw (t.$1)) (minll (t.$1) + minw (t.$2))
  CAbove -> max (minw (t.$1)) (minw (t.$2))
  CDup -> min (minw (t.$1)) (minw (t.$2))
  CJoin -> minw (t.$1)
  -----------------------------------------------------------
  CApply -> if numPars (t.$1) /= len (t.$2) then length (setErrorMsg (numPars (t.$1)) (len (t.$2))) else minw (t.$1)
  CParC -> (\(x,_,_) -> x) (head $ fillmins t)
  CIndentC -> lexemeI t + minw (t.$2)
  CBesideC -> max (minw (t.$1)) (minll (t.$1) + minw (t.$2))
  CAboveC -> max (minw (t.$1)) (minw (t.$2))
  CDupC -> min (minw (t.$1)) (minw (t.$2))
  CJoinC -> minw (t.$1)
  -----------------------------------------------------------
  CApplyC -> if numPars (t.$1) /= len (t.$2) then length (setErrorMsg (numPars (t.$1)) (len (t.$2))) else minw (t.$1)
  -----------------------------------------------------------
  CFilla -> minw (t.$1)
  CFillBlock -> minw (t.$2)
  CConsFillList -> minw (t.$1)
  CNilFillList -> minwi t
  ctor -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute minw")


minll :: Zipper PPRoot -> T_PLL
minll t = case constructor t of
  CEmpty -> 0
  CText -> length (lexemeT t)
  CIndent -> lexemeI t + minll(t.$2)
  CBeside -> minll (t.$1) + minll (t.$2)
  CAbove -> minll (t.$2)
  CDup -> min (minll (t.$1)) (minll (t.$2))
  CJoin -> minll (t.$1)
  -----------------------------------------------------------
  CApply -> if numPars (t.$1) /= len (t.$2) then length (setErrorMsg (numPars (t.$1)) (len (t.$2))) else minll (t.$1)
  CParC -> (\(_,y,_) -> y) (head $ fillmins t)
  CIndentC -> lexemeI t + minll (t.$2)
  CBesideC -> minll (t.$1) + minll (t.$2)
  CAboveC -> minll (t.$2)
  CDupC -> min (minll (t.$1)) (minll (t.$2))
  CJoinC -> minll (t.$1)
  -----------------------------------------------------------
  CApplyC -> if numPars (t.$1) /= len (t.$2) then length (setErrorMsg (numPars (t.$1)) (len (t.$2))) else minll (t.$1)
  -----------------------------------------------------------
  CFilla -> minll (t.$1)
  CFillBlock -> minll (t.$2)
  CConsFillList -> minll (t.$1)
  CNilFillList -> minlli t
  ctor -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute minll")


frame :: Zipper PPRoot -> Int -> T_FRAME
frame t f = case constructor $ parent t of
    CIndent -> narrowFrame (lexemeI $ parent t) (frame (parent t) f)
    CBeside -> if t.|1 then narrowFrame (minll (t.$<1)) (frame (parent t) f)
                       else narrowll (minw (t.$>1)) (frame (parent t) f)
    CAbove -> frame (parent t) f
    CDup -> frame (parent t) f
    CJoin -> frame (parent t) f
    ----------------------------------------------------------
    CApply -> if not $ t.|1 then frame (parent t) f
               else error ""
    CIndentC -> narrowFrame (lexemeI $ parent t) (frame (parent t) f)
    CBesideC -> if t.|1 then narrowFrame (minll (t.$<1)) (frame (parent t) f)
                       else narrowll (minw (t.$>1)) (frame (parent t) f)
    CAboveC -> frame (parent t) f
    CDupC -> frame (parent t) f
    CJoinC -> frame (parent t) f
    -----------------------------------------------------------
    CConsArgs -> if t.|1 then head $ reqs (parent t) f
                       else error ""
    -----------------------------------------------------------
    CApplyC -> if not $ t.|1 then frame (parent t) f
                       else error ""
    CConsPPCArgs -> if t.|1 then head $ ireqs (parent t) f
                       else error ""
    -----------------------------------------------------------
    CFilla -> do
      let l_pw = (\(F x _) -> x) (frame (parent t) f)
      F l_pw l_pw
    CFillBlock -> F (lexemeF $ parent t) (lexemeF $ parent t)
    CConsFillList -> frame (parent t) f
    -----------------------------------------------------------
    CBest -> F f f
    CAll -> F f f
    CDispl -> F f f

    ctor -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute frame")



narrowFrame :: Int -> T_FRAME -> T_FRAME
narrowFrame i (F s l) = F (s-i) (l-i)

narrowll :: Int -> T_FRAME -> T_FRAME
narrowll i (F s l) = F s (l-i)

-----------------------------------------------------

numPars :: Zipper PPRoot -> Int
numPars t = case constructor t of
  CParC -> 1
  CIndentC -> numPars (t.$2)
  CBesideC -> numPars (t.$1) + numPars (t.$2)
  CAboveC -> numPars (t.$1) + numPars (t.$2)
  CDupC -> numPars (t.$1)
  CJoinC -> numPars (t.$1)
  -------------------------------------------------
  CApplyC -> numPars (t.$2)
  CNilPPCArgs -> 0
  CConsPPCArgs -> numPars (t.$1) + numPars (t.$2)
  ctor -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute numPars")


fillmins :: Zipper PPRoot -> T_MINS
fillmins t | constructor t == CNilPPCArgs = []
           | constructor (parent t) == CConsPPCArgs && (t.|1) = take (numPars (t.$<1)) (fillmins $ parent t)
           | constructor t == CConsPPCArgs = (minw (t.$2), minll (t.$2), maxh (t.$2)) : fillmins (t.$1)
           | constructor (parent t) == CApply && not (t.|1) = mins (t.$>1)
           ----------------------------------------------------------------------------------------
           | constructor (parent t) == CApplyC && not (t.|1) = fillmins (t.$>1)
           ----------------------------------------------------------------------------------------
           | otherwise  = case constructor $ parent t of
  CBesideC -> if t.|1 then drop (numPars (t.$<1)) (fillmins $ parent t)
                       else take (numPars t) (fillmins $ parent t)
  CAboveC -> if t.|1 then drop (numPars (t.$<1)) (fillmins $ parent t)
                       else take (numPars t) (fillmins $ parent t)
  CDupC -> fillmins $ parent t
  CIndentC -> fillmins $ parent t
  CJoinC -> fillmins $ parent t

  ctor -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute fillmins")


mins :: Zipper PPRoot -> T_MINS
mins t = case constructor t of
  CNilArgs -> []
  CConsArgs -> (minw (t.$2), minll (t.$2), maxh (t.$2)) : mins (t.$1)
  ctor -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute mins")



reqs :: Zipper PPRoot -> Int -> T_REQS
reqs t f
  | constructor (parent t) == CApply && t.|1 = reqs (t.$<1) f
  | constructor (parent t) == CConsArgs && not (t.|1) = tail $ reqs (parent t) f
  | otherwise = case constructor t of
             CParC -> [frame t f]
             CIndentC -> reqs (t.$2) f
             CBesideC -> reqs (t.$1) f ++ reqs (t.$2) f
             CAboveC -> reqs (t.$1) f ++ reqs (t.$2) f
             CDupC -> zipWith max (reqs (t.$1) f) (reqs (t.$2) f)
             CJoinC -> reqs (t.$1) f
             --------------------------------------------------
             CApplyC -> reqs (t.$2) f
             CConsPPCArgs -> reqs (t.$1) f ++ reqs (t.$2) f
             CNilPPCArgs -> []
             ctor -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute reqs")


ifillmins :: Zipper PPRoot -> T_MINS
ifillmins t | constructor (parent t) == CApplyC && t.|1 = fillmins $ parent t
            | constructor (parent t) == CConsPPCArgs && not (t.|1) = drop (numPars (t.$>1)) (fillmins $ parent t)
            | otherwise = error ("Incorrect constructor: \"" ++ show (constructor (parent t)) ++ "\" for attribute ifillmins")


ireqs :: Zipper PPRoot -> Int -> T_REQS
ireqs t f | constructor (parent t) == CApplyC && t.|1 = reqs (t.$<1) f
          | constructor (parent t) == CConsPPCArgs && not (t.|1) = tail $ ireqs (parent t) f
          | otherwise = error ("Incorrect constructor: \"" ++ show (constructor (parent t))  ++ "\" for attribute ireqs")

-----------------------------------------------------

len :: Zipper PPRoot -> Int
len t = case constructor t of
  CNilArgs -> 0
  CConsArgs -> 1 + len (t.$1)
  -------------------------------
  CNilPPCArgs -> 0
  CConsPPCArgs -> 1 + len (t.$1)
  ctor -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute len")


setErrorMsg :: Int -> Int -> String
setErrorMsg numpars leng
  = "<Error: incorrect apply expression. #pars "
  ++ show numpars ++ " /= #args "
  ++ show leng    ++ ">"

eqSetErrorMsg :: Int -> Int -> String
eqSetErrorMsg apars bpars
  = "<Error: incorrect choice expression. #pars left "
  ++ show apars ++ " /= #pars right "
  ++ show bpars ++ ">"

---------------------------------------------------------------------
-- MaxH


maxh :: Zipper PPRoot -> T_PH
maxh t = case constructor t of
  CEmpty -> 0
  CText -> 1
  CIndent -> maxh (t.$2)
  CBeside -> besideHeight (maxh (t.$1)) (maxh (t.$2))
  CAbove -> maxh (t.$1) + maxh (t.$2)
  CDup -> max (maxh (t.$1)) (maxh (t.$2))
  CJoin -> maxh (t.$1)
  CApply -> if numPars (t.$1) /= len (t.$2) then 1 else maxh (t.$1)
  CParC -> (\(_,_,z) -> z) (head $ fillmins t)
  CIndentC -> maxh (t.$2)
  CBesideC -> besideHeight (maxh (t.$1)) (maxh (t.$2))
  CAboveC -> maxh (t.$1) + maxh (t.$2)
  CDupC -> max (maxh (t.$1)) (maxh (t.$2))
  CJoinC -> maxh (t.$1)
  CApplyC -> if numPars (t.$1) /= len (t.$2) then 1 else maxh (t.$1)
  CFilla -> maxh (t.$1)
  CFillBlock -> maxh (t.$2)
  CConsFillList -> maxh (t.$1)
  CNilFillList -> maxhi t

  ctor -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute maxh")


maxhi :: Zipper PPRoot -> T_PH
maxhi t | constructor (parent t) == CConsFillList && not (t.|1) = consHeight (maxh (t.$>1)) (maxhi (parent t)) True
        | otherwise = case constructor $ parent t of
          CFilla -> 0
          CFillBlock -> 0
          ctor -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute maxhi")

besideHeight :: Int -> Int -> Int
besideHeight lh rh
  = if lh == 0 || rh == 0 then 0 else 1


consHeight :: Int -> Int -> Bool -> Int
consHeight pPh acth avail
  | acth == 0  = if pPh > 0 then 1 else 0
  | otherwise  = acth + if avail then 0 else 1

------------------------------------------------------------------------
-- Fill

pw :: Zipper PPRoot -> Int -> T_PW
pw t f | constructor (parent t) == CConsFillList && not (t.|1) = pw (parent t) f
       | otherwise = case constructor $ parent t of
         CFilla -> (\(F x _) -> x) (frame (parent t) f)
         CFillBlock -> lexemeF $ parent t

         ctor -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute pw")

minwi :: Zipper PPRoot -> T_PW
minwi t | constructor (parent t) == CConsFillList && not (t.|1) = minwi $ parent t
        | otherwise = case constructor $ parent t of
          CFilla -> 0
          CFillBlock -> 0
          ctor -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute minwi")

minlli :: Zipper PPRoot -> T_PW
minlli t | constructor (parent t) == CConsFillList && not (t.|1) = minlli (parent t) + minwi (parent t) + minw (t.$>1)
         | otherwise = case constructor $ parent t of
          CFilla -> 0
          CFillBlock -> 0
          ctor -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute minlli")

-------------------------------------------------------

fillfmts :: Zipper PPRoot -> Int -> T_FMTS
fillfmts t f | constructor (parent t) == CApply && not (t.|1) = fmtsL (t.$>1) f
----------------------------------------------------------------------------------------
             | constructor (parent t) == CApplyC && not (t.|1) = fmtsL (t.$>1) f
             | constructor (parent t) == CConsPPCArgs && (t.|1) = take (numPars t) (ifillfmts (parent t) f)
----------------------------------------------------------------------------------------
             | otherwise = case constructor $ parent t of
               CBesideC -> if t.|1 then drop (numPars (t.$<1)) (fillfmts (parent t) f)
                         else take (numPars t) (fillfmts (parent t) f)
               CAboveC -> if t.|1 then drop (numPars (t.$<1)) (fillfmts (parent t) f)
                        else take (numPars t) (fillfmts (parent t) f)
               CDupC -> fillfmts (parent t) f
               CIndentC -> fillfmts (parent t) f
               CJoinC -> fillfmts (parent t) f
               ctor -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute fillfmts")


ifillfmts :: Zipper PPRoot -> Int -> T_FMTS
ifillfmts t f | constructor (parent t) == CApplyC && (t.|1) = fillfmts (parent t) f
              | constructor (parent t) == CConsPPCArgs && not (t.|1) = drop (numPars (t.$>1)) (ifillfmts (parent t) f)
              | otherwise = error ("Incorrect constructor: \"" ++ show (constructor (parent t)) ++ "\" for attribute ifillfmts")

fillerrs :: Zipper PPRoot -> Int -> T_ERRS
fillerrs t f | constructor (parent t) == CApply && not (t.|1) = errorL (t.$>1) f
----------------------------------------------------------------------------------------
             | constructor (parent t) == CApplyC && not (t.|1) = errorL (t.$>1) f
             | constructor (parent t) == CConsPPCArgs && (t.|1) = take (numPars t) (ifillerrs (parent t) f)
----------------------------------------------------------------------------------------
             | otherwise = case constructor $ parent t of
               CBesideC -> if t.|1 then drop (numPars (t.$<1)) (fillerrs (parent t) f)
                         else take (numPars t) (fillerrs (parent t) f)
               CAboveC -> if t.|1 then drop (numPars (t.$<1)) (fillerrs (parent t) f)
                        else take (numPars t) (fillerrs (parent t) f)
               CDupC -> fillerrs (parent t) f
               CIndentC -> fillerrs (parent t) f
               CJoinC -> fillerrs (parent t) f
               ctor -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute fillerrs")

ifillerrs :: Zipper PPRoot -> Int -> T_ERRS
ifillerrs t f | constructor (parent t) == CApplyC && (t.|1) = fillerrs (parent t) f
              | constructor (parent t) == CConsPPCArgs && not (t.|1) = drop (numPars (t.$>1)) (ifillerrs (parent t) f)
              | otherwise = error ("Incorrect constructor: \"" ++ show (constructor (parent t)) ++ "\" for attribute ifillerrs")

--------------------------------------------------------------------------
-- Formats

fmts :: Zipper PPRoot -> Int -> T_FORMATS
fmts t f = case constructor t of
  CEmpty -> setFmtsEmpty
  CText -> setFmtsText (lexemeT t) (length (lexemeT t)) (length (lexemeT t) > (\(F x _) -> x) (frame t f))
  CIndent -> setFmtsIndent (lexemeI t) (fmts (t.$2) f) ((\(F x _) -> x) (frame t f)) (lexemeI t + minw(t.$2)) (frame t f) (error' (t.$2) f)
  CBeside -> fst $ setFmtsBeside (fmts (t.$1) f) (fmts (t.$2) f) (maxh (t.$1)) (maxh (t.$2)) (frame t f) (error' (t.$1) f || error' (t.$2) f)
  CAbove -> fst $ setFmtsAbove (fmts (t.$1) f) (fmts (t.$2) f) (maxh (t.$1)) (maxh (t.$2))
  CDup -> semFmtsDup (fmts (t.$1) f) (fmts (t.$2) f) (error' (t.$1) f) (error' (t.$2) f) (min (minw (t.$1)) (minw (t.$2)))
  CJoin -> fst $ setFmtsJoin (fmts (t.$1) f) (error' (t.$1) f)
  CApply -> eqSetFmtsApply (numPars (t.$1) /= len (t.$2)) (setErrorMsg (numPars (t.$1)) (len (t.$2))) (fmts (t.$1) f)
  CParC -> head $ fillfmts t f
  CIndentC -> setFmtsIndent (lexemeI t) (fmts (t.$2) f) ((\(F x _) -> x) (frame t f)) (lexemeI t + minw(t.$2)) (frame t f) (error' (t.$2) f)
  CBesideC -> fst $ setFmtsBeside (fmts (t.$1) f) (fmts (t.$2) f) (maxh (t.$1)) (maxh (t.$2)) (frame t f) (error' (t.$1) f || error' (t.$2) f)
  CAboveC -> fst $ setFmtsAbove (fmts (t.$1) f) (fmts (t.$2) f) (maxh (t.$1)) (maxh (t.$2))
  CDupC -> semFmtsCdup (fmts (t.$1) f) (fmts (t.$2) f) (error' (t.$1) f) (error' (t.$2) f) (numPars (t.$1)) (numPars (t.$2)) (min (minw (t.$1)) (minw (t.$2))) (eqSetErrorMsg (numPars (t.$1)) (numPars (t.$2)))
  CJoinC -> fst $ setFmtsJoin (fmts (t.$1) f) (error' (t.$1) f)
  CApplyC -> eqSetFmtsApply (numPars (t.$1) /= len (t.$2)) (setErrorMsg (numPars (t.$1)) (len (t.$2))) (fmts (t.$1) f)
  CFilla -> eqSetFmtsFill (fmtsFl (t.$1) f)
  CFillBlock -> setFmtsFillblock (lexemeF t) (fmtsFl (t.$2) f) ((\(F x _) -> x) (frame t f))
  ctor -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute fmts")


fmtsi :: Zipper PPRoot -> Int -> Formats
fmtsi t f | constructor (parent t) == CConsFillList && not (t.|1) = fst $ setFmtsFilllist (fmtsi (parent t) f) (fmts (t.$>1) f) (maxhi (parent t)) (maxh (t.$>1)) (frame (parent t) f) (pw t f - (minlli t + minw (t.$>1)) >= 0)
          | otherwise = case constructor $ parent t of
            CFilla -> emptyFmts
            CFillBlock -> emptyFmts
            ctor -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute fmtsi")


fmtsFl :: Zipper PPRoot -> Int -> Formats
fmtsFl t f = case constructor t of
  CConsFillList -> fmtsFl (t.$1) f
  CNilFillList -> fmtsi t f
  ctor -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute fmtsFl")


fmtsL :: Zipper PPRoot -> Int -> T_FMTS
fmtsL t f = case constructor t of
  CNilArgs -> []
  CConsArgs -> fmts (t.$2) f : fmtsL (t.$1) f
  CNilPPCArgs -> []
  CConsPPCArgs -> fmts (t.$2) f : fmtsL (t.$1) f
  ctor -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute fmtsL")


fmtsRoot :: Zipper PPRoot -> Int -> String 
fmtsRoot t f = case constructor t of
  CBest -> eqBestFmts f (fmts (t.$1) f)
  CAll -> eqAllFmts f (fmts (t.$1) f)
  CDispl -> eqDispFmts f (fmts (t.$1) f)
  ctor -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute fmtsRoot")


afmtTxt :: String -> T_FORMATS
afmtTxt = AFormat . textFmts

setFmtsEmpty :: T_FORMATS
setFmtsEmpty = AFormat emptyFmts

setFmtsText :: String -> Int -> T_ERROR -> T_FORMATS
setFmtsText string mw err
  = afmtTxt (if err then asts mw else string)

setFmtsIndent :: Int -> T_FORMATS -> T_PW -> T_PW -> T_FRAME -> T_ERROR -> T_FORMATS
setFmtsIndent int fmt p mw fr err
  | int < 0    = afmtTxt "<Error: negative indentation>"
  | int > p    = afmtTxt . asts $ mw
  | err        = setFmtsIndent' errorIndent
  | otherwise  = setFmtsIndent' (indentFmts fr)
  where setFmtsIndent' fmt_fc
          = case fmt of
              AFormat fs -> AFormat (fmt_fc int fs)
              TFormats as bs ae be
                         -> TFormats (fmt_fc int as)
                                     (fmt_fc int bs) ae be

setFmtsBeside :: T_FORMATS -> T_FORMATS -> T_PH -> T_PH -> T_FRAME -> T_ERROR -> (T_FORMATS, Bool)
setFmtsBeside ls rs lh rh fr err
  = setFmtsAbAbove ls rs setFmtsBeside' "<Error: can't beside two pairs>"
  where setFmtsBeside' as bs
          = setFmtsAboveA (lh == 0) (rh == 0) as bs
               (if err then errorBeside
                       else besideFmts fr)

setFmtsAbove :: T_FORMATS -> T_FORMATS -> T_PH -> T_PH -> (T_FORMATS, Bool)
setFmtsAbove us ls uh lh
  = setFmtsAbAbove us ls setFmtsAbove' "<Error: can't above two pairs>"
  where setFmtsAbove' as bs = setFmtsAboveA (uh == 0) (lh == 0) as bs aboveFmts

setFmtsAboveA :: Bool -> Bool -> Formats -> Formats -> (Formats -> Formats -> Formats) -> Formats
setFmtsAboveA aempty bempty as bs fmt_fc
  | aempty = bs
  | bempty = as
  | otherwise = fmt_fc as bs

setFmtsAbAbove :: T_FORMATS -> T_FORMATS -> (Formats -> Formats -> Formats) -> String -> (T_FORMATS, Bool)
setFmtsAbAbove fs gs fmt_fc etxt
  = case fs of
      AFormat ffmts -> case gs of
                         AFormat gfmts -> ( AFormat (fmt_fc ffmts gfmts), False )
                         TFormats as bs ae be
                                       -> ( TFormats (fmt_fc ffmts as)
                                                     (fmt_fc ffmts bs) ae be
                                          , False )
      TFormats as bs ae be
                    -> case gs of
                         AFormat gfmts -> ( TFormats (fmt_fc as gfmts)
                                                     (fmt_fc bs gfmts) ae be
                                          , False )
                         _     -> ( afmtTxt etxt, True )

semFmtsDup :: T_FORMATS -> T_FORMATS -> T_ERROR -> T_ERROR -> T_PW -> T_FORMATS
semFmtsDup afs bfs ae be mw
  = if ae && be
    then afmtTxt . asts $ mw
    else
         let get_fmts fs
               = case fs of
                   AFormat as       -> as
                   TFormats {}      -> textFmts "<Error: can't dup a dup>"
             afmts = get_fmts afs
             bfmts = get_fmts bfs
         in  TFormats afmts bfmts ae be

setFmtsJoin :: T_FORMATS -> T_ERROR -> (T_FORMATS, Bool)
setFmtsJoin    (TFormats as bs ae be)  _
  = ( AFormat $ if be
                then (if null as then bs else as)
                else if ae
                     then (if null bs then as else bs)
                     else merge as bs
    , False
    )
setFmtsJoin fs@(AFormat _) err
  = if err then (fs, err)
           else (afmtTxt "<Error: can't join a single result>", True)

setFmtsApply :: T_ERROR -> T_FORMATS -> T_FORMATS -> T_FORMATS
setFmtsApply True  a  _  =  a
setFmtsApply False _  b  =  b

setFmtsFillblock :: Int -> Formats -> Int -> T_FORMATS
setFmtsFillblock int fmt w
  | int < 0     = afmtTxt "<Error: negative page width in fillblock>"
  | int > w     = afmtTxt (asts int)
  | otherwise   = AFormat fmt

eqSetFmtsApply :: T_ERROR -> String -> T_FORMATS -> T_FORMATS
eqSetFmtsApply err msg = setFmtsApply err (AFormat (textFmts msg))

eqSetFmtsFill :: Formats -> T_FORMATS
eqSetFmtsFill = AFormat

asts :: Int -> String
asts 0 = ""
asts 1 = "*"
asts s = ':' : replicate (s-2) '*' ++ ">"


semFmtsCdup :: T_FORMATS -> T_FORMATS -> T_ERROR -> T_ERROR -> Int -> Int -> T_PW -> String -> T_FORMATS
semFmtsCdup afs bfs ae be an bn mw em
  = if an /= bn then afmtTxt em
                else semFmtsDup afs bfs ae be mw


setFmtsFilllist :: Formats -> T_FORMATS -> T_PH -> T_PH -> T_FRAME -> Bool -> (Formats, Bool)
setFmtsFilllist ifmts nfmts ih nh fr avail
  = case nfmts of
      AFormat ns -> if ih == 0                       {- left operand empty?   -}
                    then (ns, False)
                    else if nh == 0                  {- right operand empty?  -}
                         then (ifmts, False)
                         else if nh <= 1
                              then ( chooseAbBesideFmts avail ifmts ns fr, False )
                              else ( chooseAbErrorBeside
                                       avail ifmts (textFmts "<Error: element in fill higher than 1>")
                                   , True )
      _  -> ( setFmtsFilllist' . textFmts $ "<Error: element in fill list is a pair>"
                    , True )
  where setFmtsFilllist' fs
          = setFmtsAboveA (ih == 0) (nh == 0) fs ifmts (choose_ab errorBeside)
        choose_ab bsd_fc = if avail then bsd_fc else aboveFmts


chooseAbBesideFmts :: Bool -> Formats -> Formats -> T_FRAME -> Formats
chooseAbBesideFmts avail fa fb f =
 if avail then besideFmts f fa fb else aboveFmts fa fb

chooseAbErrorBeside :: Bool -> Formats -> Formats -> Formats
chooseAbErrorBeside avail fa fb =
 if avail then errorBeside fa fb else aboveFmts fa fb


eqAllFmts :: T_PW -> T_FORMATS -> String
eqAllFmts p fmt =
 allf (setFmtsRender p fmt)

eqBestFmts :: T_PW -> T_FORMATS -> String
eqBestFmts p fmt =
 best (setFmtsRender p fmt)

eqDispFmts :: T_PW -> T_FORMATS -> String
eqDispFmts p fmt =
 dispf (setFmtsRender p fmt)

setFmtsRender :: T_PW -> T_FORMATS -> Formats
setFmtsRender p fmt =
 if p<0 then textFmts "<Error: negative page width >" else case fmt of { (AFormat fm) -> fm ; _ -> textFmts "<Error: can\'t render a pair>" }
--------------------------------------------------------------------------
-- Error

error' :: Zipper PPRoot -> Int -> T_ERROR
error' t f = case constructor t of
  CEmpty -> False
  CText -> length (lexemeT t) > (\(F x _) -> x) (frame t f)
  CIndent -> lexemeI t < 0 || lexemeI t > (\(F x _) -> x) (frame t f) || error' (t.$2) f
  CBeside -> error' (t.$1) f || error' (t.$2) f || snd (setFmtsBeside (fmts (t.$1) f) (fmts (t.$2) f) (maxh (t.$1)) (maxh (t.$2)) (frame t f) (error' (t.$1) f || error' (t.$2) f))
  CAbove -> error' (t.$1) f || error' (t.$2) f || snd ( setFmtsAbove (fmts (t.$1) f) (fmts (t.$2) f) (maxh (t.$1)) (maxh (t.$2)))
  CDup -> error' (t.$1) f && error' (t.$2) f
  CJoin -> error' (t.$1) f || snd (setFmtsJoin (fmts (t.$1) f) (error' (t.$1) f))
  CApply -> (numPars (t.$1) /= len (t.$2)) || error' (t.$1) f
  CParC -> head $ fillerrs t f
  CIndentC -> (lexemeI t < 0) || (lexemeI t > (\(F x _) -> x) (frame t f)) || error' (t.$2) f
  CBesideC -> error' (t.$1) f || error' (t.$2) f || snd (setFmtsBeside (fmts (t.$1) f) (fmts (t.$2) f) (maxh (t.$1)) (maxh (t.$2)) (frame t f) (error' (t.$1) f || error' (t.$2) f))
  CAboveC -> error' (t.$1) f || error' (t.$2) f || snd ( setFmtsAbove (fmts (t.$1) f) (fmts (t.$2) f) (maxh (t.$1)) (maxh (t.$2)))
  CDupC -> (numPars (t.$1) /= numPars (t.$2)) || (error' (t.$1) f && error' (t.$2) f)
  CJoinC -> error' (t.$1) f || snd (setFmtsJoin (fmts (t.$1) f) (error' (t.$1) f))
  CApplyC -> (numPars (t.$1) /= len (t.$2)) || error' (t.$1) f
  CFilla -> error' (t.$1) f
  CFillBlock -> (lexemeF t < 0) || (lexemeF t > (\(F x _) -> x) (frame t f)) || error' (t.$2) f
  CConsFillList -> error' (t.$1) f || error' (t.$2) f
  CNilFillList -> errori t f
  ctor -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute error'")

errori :: Zipper PPRoot -> Int -> T_ERROR
errori t f | constructor (parent t) == CConsFillList && not (t.|1) = errori (parent t) f || snd (setFmtsFilllist (fmtsi (parent t) f) (fmts (t.$>1) f) (maxhi (parent t)) (maxh (t.$>1)) (frame (parent t) f) (pw t f - (minlli t + minw (t.$>1)) >= 0))
           | otherwise = case constructor $ parent t of
             CFilla -> False
             CFillBlock -> False
             ctor -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute errori")

errorL :: Zipper PPRoot -> Int -> T_ERRS
errorL t  f = case constructor t of
  CNilArgs -> []
  CConsArgs -> error' (t.$2) f : errorL (t.$1) f
  CNilPPCArgs -> []
  CConsPPCArgs -> error' (t.$2) f : errorL (t.$1) f
  ctor -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute errorL")