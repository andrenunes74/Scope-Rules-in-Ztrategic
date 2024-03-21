{-# LANGUAGE GADTs,
             MultiParamTypeClasses,
             FlexibleContexts,
             FlexibleInstances, 
             DeriveDataTypeable
#-}

module PP.Memo.Pretty where

import Data.Generics.Zipper
import Data.Data

import Library.ZipperAG
import Library.Memo.AGMemo
-- import StrategicData

import PP.Memo.Core
import PP.Memo.SharedMemo


--------------------------------------------------------
----------
-------------- Attributes 
----------
--------------------------------------------------------

fmts = flip fmts'

fmts' :: (Memo Att_FMTS MemoTable T_FORMATS) => Int -> AGTree_m PPS MemoTable T_FORMATS
fmts' f = memo Att_FMTS $ \t -> case constructor t of
  CEmpty -> (setFmtsEmpty, t)
  CText ->   let (fr, t') = frame t f 
                 fmtT = setFmtsText (lexemeT t') (length (lexemeT t')) (length (lexemeT t') > (\(F x _) -> x) fr)
             in (fmtT, t') 
  CIndent -> let (currF, t'   ) = frame t f
                 (fmts2, t''  ) = fmts' f   .@. (t'.$2)
                 (minw2, t''' ) = minw      .@. (t''.$2)
                 (err2 , t'''') = error'' f .@. (t'''.$2)
                 lexI = lexemeI t''''
                 res = setFmtsIndent lexI fmts2 ((\(F x _) -> x) currF) (lexI + minw2) currF err2
             in (res, t'''')    
  CBeside -> let (fmts1, t'      ) = fmts' f   .@. (t.$1)
                 (fmts2, t''     ) = fmts' f   .@. (t'.$2)
                 (maxh1, t'''    ) = maxh      .@. (t''.$1)
                 (maxh2, t''''   ) = maxh      .@. (t'''.$2)
                 (currF, t'''''  ) = frame' f t''''
                 (errs1, t'''''' ) = error'' f .@. (t'''''.$1)
                 (errs2, t''''''') = error'' f .@. (t''''''.$2)
                 res = fst $ setFmtsBeside fmts1 fmts2 maxh1 maxh2 currF (errs1 || errs2)
             in (res, t''''''')
  CAbove  -> let (fmts1, t'   ) = fmts' f .@. (t.$1)
                 (fmts2, t''  ) = fmts' f .@. (t'.$2)
                 (maxh1, t''' ) = maxh    .@. (t''.$1)
                 (maxh2, t'''') = maxh    .@. (t'''.$2)
                 res = fst $ setFmtsAbove fmts1 fmts2 maxh1 maxh2
             in (res, t'''')
  CDup    -> let (fmts1, t'   ) = fmts' f   .@. (t.$1)
                 (fmts2, t''  ) = fmts' f   .@. (t'.$2)
                 (errs1, t''' ) = error'' f .@. (t''.$1)
                 (errs2, t'''') = error'' f .@. (t'''.$2)
                 (minw1, t''''') = minw     .@. (t''''.$1)
                 (minw2, t'''''') = minw    .@. (t'''''.$2)
                 res = semFmtsDup fmts1 fmts2 errs1 errs2 (min minw1 minw2)
             in (res, t'''''')
  CJoin   -> let (fmts1, t' ) = fmts' f   .@. (t.$1)
                 (errs1, t'') = error'' f .@. (t'.$1)
                 res = fst $ setFmtsJoin fmts1 errs1
             in (res, t'')
  CApply  -> let (np, t'     ) = numPars .@. (t.$1) 
                 (len2, t''  ) = len     .@. (t'.$2)    
                 (fmts1, t''') = fmts' f .@. (t''.$1)
                 res = eqSetFmtsApply (np /= len2) (setErrorMsg np len2) fmts1
             in (res, t''')
  CParC   -> let (fifmts, t') = fillfmts t f 
                 res = head fifmts
             in (res, t')
  CIndentC -> let (fmts1, t') = fmts' f .@. (t.$2)
                  (fram1, t'') = frame' f t'
                  (minw1, t''') = minw .@. (t''.$2)
                  (errs2, t'''') = error'' f .@. (t'''.$2)
                  lexI = lexemeI t''''
                  res = setFmtsIndent lexI fmts1 ((\(F x _) -> x) fram1) (lexI + minw1) fram1 errs2
              in (res, t'''')
  CBesideC -> let (fmts1, t'      ) = fmts' f   .@. (t.$1)
                  (fmts2, t''     ) = fmts' f   .@. (t'.$2)
                  (maxh1, t'''    ) = maxh      .@. (t''.$1)
                  (maxh2, t''''   ) = maxh      .@. (t'''.$2)
                  (currF, t'''''  ) = frame' f t''''
                  (errs1, t'''''' ) = error'' f .@. (t'''''.$1)
                  (errs2, t''''''') = error'' f .@. (t''''''.$2)
                  res = fst $ setFmtsBeside fmts1 fmts2 maxh1 maxh2 currF (errs1 || errs2)
              in (res, t''''''')
  CAboveC ->  let (fmts1, t'   ) = fmts' f .@. (t.$1)
                  (fmts2, t''  ) = fmts' f .@. (t'.$2)
                  (maxh1, t''' ) = maxh    .@. (t''.$1)
                  (maxh2, t'''') = maxh    .@. (t'''.$2)
                  res = fst $ setFmtsAbove fmts1 fmts2 maxh1 maxh2
              in (res, t'''')
  CDupC   ->  let (fmts1, t') = fmts' f .@. (t.$1)
                  (fmts2, t'') = fmts' f .@. (t'.$2)
                  (errs1, t''') = error'' f .@. (t''.$1)
                  (errs2, t'''') = error'' f .@. (t'''.$2)
                  (np1  , t''''') = numPars .@. (t''''.$1)
                  (np2  , t'''''') = numPars .@. (t'''''.$2)
                  (minw1, t''''''') = minw .@. (t''''''.$1)
                  (minw2, t'''''''') = minw .@. (t'''''''.$2)
                  res = semFmtsCdup fmts1 fmts2 errs1 errs2 np1 np2 (min minw1 minw2) (eqSetErrorMsg np1 np2)
              in (res, t'''''''')  
  CJoinC -> let  (fmts1, t' ) = fmts' f   .@. (t.$1)
                 (errs1, t'') = error'' f .@. (t'.$1)
                 res = fst $ setFmtsJoin fmts1 errs1
            in (res, t'')
  CApplyC -> let (np, t'     ) = numPars .@. (t.$1) 
                 (len2, t''  ) = len     .@. (t'.$2)    
                 (fmts1, t''') = fmts' f .@. (t''.$1)
                 res = eqSetFmtsApply (np /= len2) (setErrorMsg np len2) fmts1
             in (res, t''')
  CFilla -> let (f1, t') = fmtsFl' f .@. (t.$1)
                res = eqSetFmtsFill f1
            in (res, t')
  CFillBlock -> let (ffl2, t' ) = fmtsFl' f .@. (t.$2)
                    (fr  , t'') = frame' f t'
                    res = setFmtsFillblock (lexemeF t) ffl2 ((\(F x _) -> x) fr)
                in (res, t'')
  ctor       -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute fmts")


error' = flip error''


error'' :: (Memo Att_ERROR MemoTable T_ERROR) => Int -> AGTree_m PPS MemoTable T_ERROR
error'' f = memo Att_ERROR $ \t -> case constructor t of
  CEmpty        -> (False, t)
  CText         -> let (fr, t') = frame' f t 
                       res = length (lexemeT t) > (\(F x _) -> x) fr
                   in (res, t')
  CIndent       -> let (fr, t') = frame' f t 
                       (e2, t'') = error'' f .@. (t'.$2)
                       res =   lexemeI t < 0 || lexemeI t > (\(F x _) -> x) fr || e2
                   in (res, t'')
  CBeside       -> let (e1, t'      ) = error'' f .@. (t.$1)
                       (e2, t''     ) = error'' f .@. (t'.$2)
                       (f1, t'''    ) = fmts'   f .@. (t''.$1)
                       (f2, t''''   ) = fmts'   f .@. (t'''.$2)
                       (m1, t'''''  ) = maxh      .@. (t''''.$1)
                       (m2, t'''''' ) = maxh      .@. (t'''''.$2)
                       (fr, t''''''') = frame' f       t''''''
                       res = e1 || e2 || snd (setFmtsBeside f1 f2 m1 m2 fr (e1 || e2))
                   in (res, t''''''')
  CAbove        -> let (e1, t'     ) = error'' f .@. (t.$1)
                       (e2, t''    ) = error'' f .@. (t'.$2)
                       (f1, t'''   ) = fmts'   f .@. (t''.$1)
                       (f2, t''''  ) = fmts'   f .@. (t'''.$2)
                       (m1, t''''' ) = maxh      .@. (t''''.$1)
                       (m2, t'''''') = maxh      .@. (t'''''.$2)
                       res = e1 || e2 || snd (setFmtsAbove f1 f2 m1 m2)
                in (res, t'''''')
  CDup          -> let (e1, t' ) = error'' f .@. (t.$1)
                       (e2, t'') = error'' f .@. (t'.$2)
                       res = e1 && e2 
                   in (res, t'')
  CJoin         -> let (e1, t' ) = error'' f .@. (t.$1)
                       (fm, t'') = fmts' f .@. (t'.$1)
                       res = e1 || snd (setFmtsJoin fm e1)
                  in (res, t'')
  CApply        -> let (np, t'  ) = numPars   .@. (t.$1)
                       (le, t'' ) = len       .@. (t'.$2) 
                       (er, t''') = error'' f .@. (t''.$1)
                       res        = (np /= le) || er
                   in (res, t''')
  CParC         -> let (flr, t') = fillerrs' f t 
                       res = head flr 
                   in (res, t')  
  CIndentC      -> let (fr, t' ) = frame' f t 
                       (er, t'') = error'' f .@. (t.$2)
                       res = (lexemeI t < 0) || (lexemeI t > (\(F x _) -> x) fr) || er
                   in (res, t'')
  CBesideC      -> let (e1, t'      ) = error'' f .@. (t.$1)
                       (e2, t''     ) = error'' f .@. (t'.$2)
                       (f1, t'''    ) = fmts'   f .@. (t''.$1)
                       (f2, t''''   ) = fmts'   f .@. (t'''.$2)
                       (m1, t'''''  ) = maxh      .@. (t''''.$1)
                       (m2, t'''''' ) = maxh      .@. (t'''''.$2)
                       (fr, t''''''') = frame' f       t'''''' 
                       res = e1 || e2 || snd (setFmtsBeside f1 f2 m1 m2 fr (e1 || e2))
                   in (res, t''''''')
  CAboveC       -> let (e1, t'     ) = error'' f .@. (t.$1)
                       (e2, t''    ) = error'' f .@. (t'.$2)
                       (f1, t'''   ) = fmts'   f .@. (t''.$1)
                       (f2, t''''  ) = fmts'   f .@. (t'''.$2)
                       (m1, t''''' ) = maxh      .@. (t''''.$1)
                       (m2, t'''''') = maxh      .@. (t'''''.$2)
                       res = e1 || e2 || snd (setFmtsAbove f1 f2 m1 m2)
                   in (res, t'''''') 
  CDupC         -> let (np1, t'   ) = numPars   .@. (t.$1)
                       (np2, t''  ) = numPars   .@. (t'.$2)
                       (e1 , t''' ) = error'' f .@. (t''.$1)
                       (e2 , t'''') = error'' f .@. (t'''.$2)
                       res = (np1 /= np2) || (e1 && e2)
                   in (res, t'''') 
  CJoinC        -> let (er1, t' ) = error'' f .@. (t.$1)
                       (fm1, t'') = fmts'   f .@. (t'.$1)
                       res = er1 || snd (setFmtsJoin fm1 er1)
                   in (res, t'')
  CApplyC       -> let (np1, t'  ) = numPars   .@. (t.$1)
                       (le2, t'' ) = len       .@. (t'.$2)
                       (e1 , t''') = error'' f .@. (t''.$1)
                       res = (np1 /= le2) || e1
                   in (res, t''')
  CFilla        -> error'' f .@. (t.$1)
  CFillBlock    -> let (frm, t' ) = frame' f t
                       (er2, t'') = error'' f .@. (t'.$2)
                       res = (lexemeF t'' < 0) || (lexemeF t'' > (\(F x _) -> x) frm) || er2
                   in (res, t'')
  CConsFillList -> let (e1, t' ) = error'' f .@. (t.$1)
                       (e2, t'') = error'' f .@. (t'.$2)
                       res = e1 || e2
                   in (res, t'') 
  CNilFillList  -> errori t f
  ctor -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute error'")
 
frame = flip frame'

frame' :: (Memo Att_FRAME MemoTable T_FRAME) => Int -> AGTree_m PPS MemoTable T_FRAME
frame' f = memo Att_FRAME $ \t -> case constructor $ parent t of
    CIndent -> let (fr, t') = frame' f `atParent` t 
                   res = narrowFrame (lexemeI $ parent t) fr
               in (res, t')
    CBeside -> if t.|1 then let (ml, t' ) = minll    `atLeft`   t 
                                (fr, t'') = frame' f `atParent` t'
                                res = narrowFrame ml fr
                            in (res, t'')
                       else let (mw, t' )  = minw    `atRight`  t 
                                (fr, t'') = frame' f `atParent` t'
                                res = narrowll mw fr
                            in (res, t'')
    CAbove -> frame' f `atParent` t
    CDup   -> frame' f `atParent` t
    CJoin  -> frame' f `atParent` t
    ----------------------------------------------------------
    CApply -> if not $ t.|1 then frame' f `atParent` t
               else error ""
    CIndentC -> let (fr, t') = frame' f `atParent` t
                    res = narrowFrame (lexemeI $ parent t) fr
                in (res, t')    
    CBesideC -> if t.|1 then let (ml, t' ) = minll    `atLeft`   t 
                                 (fr, t'') = frame' f `atParent` t'
                                 res = narrowFrame ml fr
                             in (res, t'')
                        else let (mw, t' )  = minw    `atRight`  t 
                                 (fr, t'') = frame' f `atParent` t'
                                 res = narrowll mw fr
                             in (res, t'')

    CAboveC -> frame' f `atParent` t
    CDupC   -> frame' f `atParent` t
    CJoinC  -> frame' f `atParent` t
    -----------------------------------------------------------
    CConsArgs -> if t.|1 then let (re, t') = reqs' f `atParent` t 
                                  res = head re 
                              in (res, t')
                         else error ""
    -----------------------------------------------------------
    CApplyC -> if not $ t.|1 then frame' f `atParent` t
                             else error ""
    CConsPPCArgs -> if t.|1 then let (ir, t') = ireqs' f `atParent` t 
                                     res = head ir 
                                 in (res, t')
                            else error ""
    -----------------------------------------------------------
    CFilla -> let (fr, t') = frame' f `atParent` t 
                  l_pw     = (\(F x _) -> x) fr
                  res      = F l_pw l_pw
              in (res, t')
    CFillBlock -> (F (lexemeF $ parent t) (lexemeF $ parent t), t)
    CConsFillList -> frame' f `atParent` t
    -----------------------------------------------------------
    CBest -> (F f f, t)
    CAll -> (F f f, t)
    CDispl ->  (F f f, t)
    ctor -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute frame")
    


maxh :: (Memo Att_MAXH MemoTable T_PH) => AGTree_m PPS MemoTable T_PH
maxh =  memo Att_MAXH $ \t -> case constructor t of
  CEmpty -> (0, t)
  CText -> (1, t)
  CIndent -> maxh .@. (t.$2)
  CBeside -> let (m1, t' ) = maxh .@. (t.$1)
                 (m2, t'') = maxh .@. (t'.$2)
                 res = besideHeight m1 m2
             in (res, t'')
  CAbove -> let (m1, t' ) = maxh .@. (t.$1)
                (m2, t'') = maxh .@. (t'.$2)
                res = m1 + m2
            in (res, t'')
  CDup -> let (m1, t' ) = maxh .@. (t.$1)
              (m2, t'') = maxh .@. (t'.$2)
              res = max m1 m2
          in (res, t'')
  CJoin -> maxh .@. (t.$1)
  CApply -> let (np, t'  ) = numPars .@. (t.$1)
                (le, t'' ) = len     .@. (t'.$2)
                (mh, t''') = maxh    .@. (t''.$1)
            in if np == le then (1, t'') else (mh, t''')
  CParC -> let (fm, t') = fillmins t 
               res = (\(_,_,z) -> z) $ head fm 
           in (res, t') 
  CIndentC -> maxh .@. (t.$2)
  CBesideC -> let (m1, t' ) = maxh .@. (t.$1)
                  (m2, t'') = maxh .@. (t'.$2)
                  res = besideHeight m1 m2
              in (res, t'')
  CAboveC -> let (m1, t' ) = maxh .@. (t.$1)
                 (m2, t'') = maxh .@. (t'.$2)
                 res = m1 + m2
             in (res, t'')
  CDupC -> let (m1, t' ) = maxh .@. (t.$1)
               (m2, t'') = maxh .@. (t'.$2)
               res = max m1 m2
           in (res, t'')
  CJoinC -> maxh .@. (t.$1)
  CApplyC -> let (np, t'  ) = numPars .@. (t.$1)
                 (le, t'' ) = len     .@. (t'.$2)
                 (mh, t''') = maxh    .@. (t''.$1)
             in if np == le then (1, t'') else (mh, t''')
  CFilla -> maxh .@. (t.$1)
  CFillBlock -> maxh .@. (t.$2)
  CConsFillList -> maxh .@. (t.$1)
  CNilFillList -> maxhi t
  ctor -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute maxh")

minw :: (Memo Att_MINW MemoTable T_PW) => AGTree_m PPS MemoTable T_PW
minw = memo Att_MINW $ \t -> case constructor t of
  CEmpty -> (0, t)
  CText -> (length (lexemeT t), t)
  CIndent -> let (mw, t') = minw .@. (t.$2)
                 res = lexemeI t + mw
             in (res, t')
  CBeside -> let (mw, t'  ) = minw  .@. (t.$1)
                 (ml, t'' ) = minll .@. (t'.$1)
                 (m2, t''') = minw  .@. (t''.$2)
                 res = max mw (ml + m2)
             in (res, t''')
  CAbove -> let (mw, t' ) = minw .@. (t.$1)
                (m2, t'') = minw .@. (t'.$2)
                res = max mw m2
            in (res, t'')
  CDup -> let (mw, t' ) = minw .@. (t.$1)
              (m2, t'') = minw .@. (t'.$2)
              res = min mw m2
          in (res, t'')
  CJoin -> minw .@. (t.$1)
  -----------------------------------------------------------
  CApply -> let (np, t'  ) = numPars .@. (t.$1)
                (le, t'' ) = len     .@. (t'.$2)
                (mw, t''') = minw    .@. (t''.$1)
                res = if np /= le then length (setErrorMsg np le) else mw
            in (res, t''')
  CParC -> let (fm, t') = fillmins t
               res      = (\(x,_,_) -> x) (head $ fm)
           in (res, t') 
  CIndentC -> let (mw, t') = minw .@. (t.$2)
                  res      = lexemeI t + mw
              in (res, t') 
  CBesideC -> let (mw, t'  ) = minw  .@. (t.$1)
                  (ml, t'' ) = minll .@. (t'.$1)
                  (m2, t''') = minw  .@. (t''.$2)
                  res = max mw (ml + m2)
              in (res, t''')
  CAboveC -> let (mw, t' ) = minw .@. (t.$1)
                 (m2, t'') = minw .@. (t'.$2)
                 res = max mw m2
             in (res, t'')
  CDupC -> let (mw, t' ) = minw .@. (t.$1)
               (m2, t'') = minw .@. (t'.$2)
               res = min mw m2
           in (res, t'')
  CJoinC -> minw .@. (t.$1)
  -----------------------------------------------------------
  CApplyC -> let (np, t'  ) = numPars .@. (t.$1)
                 (le, t'' ) = len     .@. (t'.$2)
                 (mw, t''') = minw    .@. (t''.$1)
                 res = if np /= le then length (setErrorMsg np le) else mw
             in (res, t''')
  -----------------------------------------------------------
  CFilla -> minw .@. (t.$1)
  CFillBlock -> minw .@. (t.$2)
  CConsFillList -> minw .@. (t.$1)
  CNilFillList -> minwi t
  ctor -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute minw")



minll :: (Memo Att_MINLL MemoTable T_PLL) => AGTree_m PPS MemoTable T_PLL
minll = memo Att_MINLL $ \t -> case constructor t of
  CEmpty -> (0, t)
  CText -> (length (lexemeT t), t)
  CIndent -> let (ml, t') = minll .@. (t.$2)
                 res = lexemeI t + ml
             in (res, t')       
  CBeside -> let (m1, t' ) = minll .@. (t.$1)
                 (m2, t'') = minll .@. (t'.$2)
                 res = m1 + m2
             in (res, t'') 
  CAbove -> minll .@. (t.$2)
  CDup -> let (m1, t' ) = minll .@. (t.$1)
              (m2, t'') = minll .@. (t'.$2)
              res = min m1 m2
          in (res, t'')
  CJoin -> minll .@. (t.$1)
  -----------------------------------------------------------
  CApply -> let (np, t'  ) = numPars .@. (t.$1)
                (le, t'' ) = len     .@. (t'.$2)
                (ml, t''') = minll   .@. (t''.$1)
                res = if np /= le then length (setErrorMsg np le) else ml
            in (res, t''')
  CParC -> let (fm, t') = fillmins t
               res = (\(_,y,_) -> y) (head fm)
           in (res, t')
  CIndentC -> let (ml, t') = minll .@. (t.$2)
                  res = lexemeI t + ml
              in (res, t')
  CBesideC -> let (m1, t' ) = minll .@. (t.$1)
                  (m2, t'') = minll .@. (t'.$2)
                  res = m1 + m2
              in (res, t'') 
  CAboveC -> minll .@. (t.$2)
  CDupC -> let (m1, t' ) = minll .@. (t.$1)
               (m2, t'') = minll .@. (t'.$2)
               res = min m1 m2
           in (res, t'')
  CJoinC -> minll .@. (t.$1)
  -----------------------------------------------------------
  CApplyC -> let (np, t'  ) = numPars .@. (t.$1)
                 (le, t'' ) = len     .@. (t'.$2)
                 (ml, t''') = minll   .@. (t''.$1)
                 res = if np /= le then length (setErrorMsg np le) else ml
             in (res, t''')
  -----------------------------------------------------------
  CFilla -> minll .@. (t.$1)
  CFillBlock -> minll .@. (t.$2)
  CConsFillList -> minll .@. (t.$1)
  CNilFillList -> minlli t
  ctor -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute minll")

fmtsL = flip fmtsL'

fmtsL' :: (Memo Att_FMTSL MemoTable T_FMTS) => Int -> AGTree_m PPS MemoTable T_FMTS
fmtsL' f = memo Att_FMTSL $ \t -> case constructor t of
  CNilArgs     -> ([], t)
  CConsArgs    -> let (fmts2, t' ) = fmts'  f .@. (t.$2)
                      (fmtsl, t'') = fmtsL' f .@. (t'.$1)
                      res          = fmts2 : fmtsl
                  in (res, t'')
  CNilPPCArgs  -> ([], t)
  CConsPPCArgs -> let (fmts2, t' ) = fmts'  f .@. (t.$2)
                      (fmtsl, t'') = fmtsL' f .@. (t'.$1)
                      res          = fmts2 : fmtsl
                  in (res, t'')
  ctor         -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute fmtsL")


fillfmts = flip fillfmts'

fillfmts' :: (Memo Att_FILLFMTS MemoTable T_FMTS) => Int -> AGTree_m PPS MemoTable T_FMTS
fillfmts' f = memo Att_FILLFMTS $ \t -> case (constructor (parent t), (t.|1)) of 
  (CApply, False) -> fmtsL' f `atRight` t
  (CApplyC, False) -> fmtsL' f `atRight` t
  (CConsPPCArgs, True) -> let (np, t') = numPars t 
                              (iffmts, t'') = ifillfmts' f `atParent` t'
                          in (take np iffmts, t'')
  _ -> case constructor $ parent t of
         CBesideC -> if t.|1 then let (np, t')      = numPars `atLeft` t
                                      (fifmts, t'') = fillfmts' f `atParent` t'
                                      res           = drop np fifmts
                                  in (res, t'')
                             else let (np, t')      = numPars t
                                      (fifmts, t'') = fillfmts' f `atParent` t'
                                      res = take np fifmts
                                  in (res, t'')

         CAboveC -> if t.|1 then let (np, t')      = numPars `atLeft` t
                                     (fifmts, t'') = fillfmts' f `atParent` t'
                                     res           = drop np fifmts
                                  in (res, t'')
                            else let (np, t')      = numPars t
                                     (fifmts, t'') = fillfmts' f `atParent` t'
                                     res = take np fifmts
                                  in (res, t'')
         CDupC -> fillfmts' f `atParent` t
         CIndentC -> fillfmts' f `atParent` t
         CJoinC -> fillfmts' f `atParent` t
         ctor -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute fillfmts")

ifillfmts = flip ifillfmts'

ifillfmts' :: (Memo Att_IFILLFMTS MemoTable T_FMTS) => Int -> AGTree_m PPS MemoTable T_FMTS
ifillfmts' f = memo Att_IFILLFMTS $ \t -> case (constructor (parent t), (t.|1)) of 
  (CApplyC, True)       -> fillfmts' f `atParent` t 
  (CConsPPCArgs, False) -> let (np, t')       = numPars      `atRight`  t 
                               (ififmts, t'') = ifillfmts' f `atParent` t'
                               res            = drop np ififmts
                           in (res, t'')  
  _                     -> error ("Incorrect constructor: \"" ++ show (constructor (parent t)) ++ "\" for attribute ifillfmts")




numPars :: (Memo Att_NUMPARS MemoTable Int) => AGTree_m PPS MemoTable Int
numPars = memo Att_NUMPARS $ \t -> case constructor t of
  CParC        -> (1, t)
  CIndentC     -> numPars .@. (t.$2)
  CBesideC     -> let (np1, t' ) = numPars .@. (t.$1)
                      (np2, t'') = numPars .@. (t'.$2)  
                  in (np1+np2, t'')
  CAboveC      -> let (np1, t' ) = numPars .@. (t.$1)
                      (np2, t'') = numPars .@. (t'.$2)  
                  in (np1+np2, t'')
  CDupC        -> numPars .@. (t.$1)
  CJoinC       -> numPars .@. (t.$1)
  -------------------------------------------------
  CApplyC      -> numPars .@. (t.$2)
  CNilPPCArgs  -> (0, t)
  CConsPPCArgs -> let (np1, t' ) = numPars .@. (t.$1)
                      (np2, t'') = numPars .@. (t'.$2)  
                  in (np1+np2, t'')
  ctor -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute numPars")



len :: (Memo Att_LEN MemoTable Int) => AGTree_m PPS MemoTable Int
len = memo Att_LEN $ \t -> case constructor t of
  CNilArgs     -> (0, t)
  CConsArgs    -> let (l, t') = len .@. (t.$1)
                      res = 1+l
                  in (res, t') 
  -------------------------------
  CNilPPCArgs  -> (0, t)
  CConsPPCArgs -> let (l, t') = len .@. (t.$1)
                      res = 1+l
                  in (res, t') 
  ctor -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute len")


fmtsFl = flip fmtsFl'

fmtsFl' :: (Memo Att_FMTSFL MemoTable Formats) => Int -> AGTree_m PPS MemoTable Formats
fmtsFl' f = memo Att_FMTSFL $ \t -> case constructor t of
  CConsFillList -> fmtsFl' f .@. (t.$1)
  CNilFillList -> fmtsi t f
  ctor -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute fmtsFl")

fmtsi = flip fmtsi'



fmtsi' :: (Memo Att_FMTSI MemoTable Formats) => Int -> AGTree_m PPS MemoTable Formats
fmtsi' f = memo Att_FMTSI $ \t -> case (constructor (parent t), (t.|1)) of 
  (CConsFillList, False) -> let (fiP    , t'       ) = fmtsi' f `atParent` t
                                (fR     , t''      ) = fmts' f  `atRight`  t' 
                                (mhiP   , t'''     ) = maxhi    `atParent` t''
                                (mhR    , t''''    ) = maxh     `atRight`  t'''
                                (frP    , t'''''   ) = frame' f `atParent` t''''
                                (pwRes  , t''''''  ) = pw' f               t'''''
                                (mlliRes, t''''''' ) = minlli              t''''''
                                (minwR  , t'''''''') = minw     `atRight`  t'''''''
                                res = fst $ setFmtsFilllist fiP fR mhiP mhR frP (pwRes - (mlliRes + minwR) >= 0)
                            in (res, t'''''''')
  (CFilla, _) -> (emptyFmts, t)
  (CFillBlock, _) -> (emptyFmts, t)
  (ctor, _) -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute fmtsi")


pw = flip pw'

pw' :: (Memo Att_PW MemoTable T_PW) => Int -> AGTree_m PPS MemoTable T_PW
pw' f = memo Att_PW $ \t -> case (constructor (parent t), (t.|1)) of 
  (CConsFillList, False) -> pw' f `atParent` t 
  (CFilla, _)            -> let (frameP, t') = frame' f `atParent` t 
                                res = (\(F x _) -> x) frameP
                            in (res, t')
  (CFillBlock, _)        -> (lexemeF $ parent t, t)
  (ctor, _)              -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute pw")


minlli :: (Memo Att_MINLLI MemoTable T_PW) => AGTree_m PPS MemoTable T_PW
minlli = memo Att_MINLLI $ \t -> case (constructor (parent t), (t.|1)) of 
  (CConsFillList, False) -> let (mlliP , t')  = minlli `atParent` t 
                                (minwiP, t'')  = minwi `atParent` t'
                                (minwR , t''') = minw  `atRight`  t''
                                res = mlliP + minwiP + minwR
                            in (res, t''')
  (CFilla, _)            -> (0, t)
  (CFillBlock, _)        -> (0, t)
  (ctor, _) -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute minlli")



minwi :: (Memo Att_MINWI MemoTable T_PW) => AGTree_m PPS MemoTable T_PW
minwi = memo Att_MINWI $ \t -> case (constructor (parent t), (t.|1)) of 
  (CConsFillList, False) -> minwi `atParent` t 
  (CFilla, _)            -> (0, t)
  (CFillBlock, _)        -> (0, t)
  ctor -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute minwi")


maxhi :: (Memo Att_MAXHI MemoTable T_PH) => AGTree_m PPS MemoTable T_PH
maxhi = memo Att_MAXHI $ \t -> case (constructor (parent t), (t.|1)) of 
  (CConsFillList, False) -> let (maxhR, t') = maxh `atRight` t 
                                (mxhiP, t'') = maxhi `atParent` t
                                res = consHeight maxhR mxhiP True
                            in (res, t'')
  (CFilla, _)            -> (0, t)
  (CFillBlock, _)        -> (0, t)
  ctor -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute maxhi")

fillerrs = flip fillerrs'

fillerrs' :: (Memo Att_FILLERRS MemoTable T_ERRS) => Int -> AGTree_m PPS MemoTable T_ERRS
fillerrs' f = memo Att_FILLERRS $ \t -> case (constructor (parent t), (t.|1)) of 
  (CApply, False) -> errorL' f `atRight` t 
  (CApplyC, False) -> errorL' f `atRight` t 
  (CConsPPCArgs, True) -> let (np, t' ) = numPars t
                              (iF, t'') = ifillerrs' f `atParent` t'
                              res = take np iF 
                          in (res, t'')  
  (CBesideC, _) -> if (t.|1) then let (np, t') = numPars `atLeft` t 
                                      (fi, t'') = fillerrs' f `atParent` t
                                      res = drop np fi 
                                  in (res, t'')  
                             else let (np, t') = numPars t 
                                      (fi, t'') = fillerrs' f `atParent` t
                                      res = take np fi 
                                  in (res, t'')  
  (CAboveC, _) -> if (t.|1) then let (np, t') = numPars `atLeft` t 
                                     (fi, t'') = fillerrs' f `atParent` t
                                     res = drop np fi 
                                  in (res, t'')  
                             else let (np, t') = numPars t 
                                      (fi, t'') = fillerrs' f `atParent` t
                                      res = take np fi 
                                  in (res, t'') 
  (CDupC, _)    -> fillerrs' f `atParent` t
  (CIndentC, _) -> fillerrs' f `atParent` t
  (CJoinC, _)   -> fillerrs' f `atParent` t
  ctor -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute fillerrs")


ifillerrs = flip ifillerrs'

ifillerrs' :: (Memo Att_IFILLERRS MemoTable T_ERRS) => Int -> AGTree_m PPS MemoTable T_ERRS
ifillerrs' f = memo Att_IFILLERRS $ \t -> case (constructor (parent t), (t.|1)) of 
  (CApplyC, True)       -> fillerrs' f `atParent` t 
  (CConsPPCArgs, False) -> let (np, t' ) = numPars `atRight` t
                               (iF, t'') = ifillerrs' f `atParent` t
                               res = drop np iF 
                           in (res, t'')  
  (ctor, _)             ->  error ("Incorrect constructor: \"" ++ show (constructor (parent t)) ++ "\" for attribute ifillerrs")


errorL = flip errorL'


errorL' :: (Memo Att_ERRORL MemoTable T_ERRS) => Int -> AGTree_m PPS MemoTable T_ERRS
errorL' f = memo Att_ERRORL $ \t -> case constructor t of
  CNilArgs -> ([], t)
  CConsArgs -> let (er, t' ) = error'' f .@. (t.$2)
                   (eL, t'') = errorL' f .@. (t.$1)
                   res = er : eL 
               in (res, t'')  
  CNilPPCArgs -> ([], t)
  CConsPPCArgs -> let (er, t' ) = error'' f .@. (t.$2)
                      (eL, t'') = errorL' f .@. (t.$1)
                      res = er : eL 
                  in (res, t'') 
  ctor -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute errorL")

errori = flip errori'

errori' :: (Memo Att_ERRORI MemoTable T_ERROR) => Int -> AGTree_m PPS MemoTable T_ERROR
errori' f = memo Att_ERRORI $ \t -> case (constructor (parent t), (t.|1)) of 
  (CConsFillList, False) -> let (ei, t'        ) = errori' f `atParent` t
                                (fi, t''       ) = fmtsi'  f `atParent` t'
                                (fR, t'''      ) = fmts'   f `atRight`  t''
                                (mi, t''''     ) = maxhi     `atParent` t'''
                                (mR, t'''''    ) = maxh      `atRight`  t''''
                                (fr, t''''''   ) = frame' f  `atParent` t'''''
                                (pr, t'''''''  ) = pw'     f            t''''''
                                (ml, t'''''''' ) = minlli               t'''''''
                                (mn, t''''''''') = minw      `atRight`  t''''''''
                                res = ei || snd (setFmtsFilllist fi fR mi mR fr (pr - (ml + mn) >= 0))
                             in (res, t''''''''')
  (CFilla       , _    ) -> (False, t)
  (CFillBlock   , _    ) -> (False, t)
  (ctor         , _    ) -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute errori")


reqs = flip reqs'


reqs' :: (Memo Att_REQS MemoTable T_REQS) => Int -> AGTree_m PPS MemoTable T_REQS
reqs' f = memo Att_REQS $ \t -> case (constructor (parent t), (t.|1), constructor t) of 
  (CApply   , True , _           ) -> reqs' f `atLeft` t 
  (CConsArgs, False, _           ) -> let (re, t') = reqs' f `atParent` t 
                                          res = tail re 
                                      in (res, t')
  (_        , _    , CParC       ) -> let (fr, t') = frame' f t 
                                          res = [fr]
                                      in (res, t')
  (_        , _    , CIndentC    ) -> reqs' f .@. (t.$2)
  (_        , _    , CBesideC    ) -> let (r1, t' ) = reqs' f .@. (t.$1)
                                          (r2, t'') = reqs' f .@. (t'.$2)
                                          res = r1 ++ r2 
                                      in (res, t'')
  (_        , _    , CAboveC     ) -> let (r1, t' ) = reqs' f .@. (t.$1)
                                          (r2, t'') = reqs' f .@. (t'.$2)
                                          res = r1 ++ r2 
                                      in (res, t'')
  (_        , _    , CDupC      ) -> let (r1, t' ) = reqs' f .@. (t.$1)
                                         (r2, t'') = reqs' f .@. (t'.$2)
                                         res = zipWith max r1 r2
                                     in (res, t'')
  (_        , _    , CJoinC      ) -> reqs' f .@. (t.$1)
  --------------------------------------------------
  (_        , _    , CApplyC     ) -> reqs' f .@. (t.$2)
  (_        , _    , CConsPPCArgs) -> let (r1, t' ) = reqs' f .@. (t.$1)
                                          (r2, t'') = reqs' f .@. (t'.$2)
                                          res = r1 ++ r2 
                                      in (res, t'')
  (_        , _    , CNilPPCArgs ) -> ([], t)
  (_        , _    , ctor        ) -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute reqs")

ireqs = flip ireqs'


ireqs' :: (Memo Att_IREQS MemoTable T_REQS) => Int -> AGTree_m PPS MemoTable T_REQS
ireqs' f = memo Att_IREQS $ \t -> case (constructor (parent t), (t.|1)) of 
  (CApplyC     , True ) -> reqs' f `atLeft` t 
  (CConsPPCArgs, False) -> let (ir, t') = ireqs' f `atParent` t 
                               res = tail ir 
                           in (res, t') 
  _                     -> error ("Incorrect constructor: \"" ++ show (constructor (parent t))  ++ "\" for attribute ireqs")


fillmins :: (Memo Att_FILLMINS MemoTable T_MINS) => AGTree_m PPS MemoTable T_MINS
fillmins = memo Att_FILLMINS $ \t -> case (constructor (parent t), (t.|1), constructor t) of 
  (_, _, CNilPPCArgs) -> ([], t)
  (CConsPPCArgs, True, _) -> let (np, t' ) = numPars  `atLeft` t 
                                 (fp, t'') = fillmins `atParent` t'
                                 res = take np fp 
                             in (res, t'')
  (_, _, CConsPPCArgs) -> let (mw, t'   ) = minw     .@. (t.$2)
                              (ml, t''  ) = minll    .@. (t'.$2)
                              (mh, t''' ) = maxh     .@. (t''.$2)
                              (fm, t'''') = fillmins .@. (t'''.$1)
                              res = (mw, ml, mh) : fm
                          in (res, t'''')
  (CApply, False, _) -> mins `atRight` t 
  ----------------------------------------------------------------------------------------
  (CApplyC, False, _) -> fillmins `atRight` t
  ----------------------------------------------------------------------------------------
  (CBesideC, _, _) -> if t.|1 then let (np, t' ) = numPars `atLeft` t 
                                       (fm, t'') = fillmins `atParent` t'
                                       res = drop np fm
                                   in (res, t'')
                              else let (np, t' ) = numPars t 
                                       (fm, t'') = fillmins `atParent` t'
                                       res = take np fm
                                   in (res, t'')
  (CAboveC, _, _)  -> if t.|1 then let (np, t' ) = numPars `atLeft` t 
                                       (fm, t'') = fillmins `atParent` t'
                                       res = drop np fm
                                   in (res, t'')
                              else let (np, t' ) = numPars t 
                                       (fm, t'') = fillmins `atParent` t'
                                       res = take np fm
                                   in (res, t'')
  (CDupC, _, _)     -> fillmins `atParent` t
  (CIndentC, _, _)  -> fillmins `atParent` t
  (CJoinC, _, _)    -> fillmins `atParent` t
  (_, _, ctor) -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute fillmins")


mins :: (Memo Att_MINS MemoTable T_MINS) => AGTree_m PPS MemoTable T_MINS
mins = memo Att_MINS $ \t -> case constructor t of
  CNilArgs -> ([], t)
  CConsArgs -> let (mw, t'   ) = minw  .@. (t.$2)
                   (ml, t''  ) = minll .@. (t'.$2)
                   (mh, t''' ) = maxh  .@. (t''.$2)
                   (fm, t'''') = mins  .@. (t'''.$1)
                   res = (mw, ml, mh) : fm
               in (res, t'''')
  ctor -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute mins")

--------------------------------------------------------
----------
-------------- Should these be memoized? 
----------
--------------------------------------------------------

fmtsRoot :: Zipper (PPS MemoTable) -> Int -> String 
fmtsRoot t f = case constructor t of
  CBest  -> eqBestFmts f $ fst (fmts (t.$1) f)
  CAll   -> eqAllFmts  f $ fst (fmts (t.$1) f)
  CDispl -> eqDispFmts f $ fst (fmts (t.$1) f)
  ctor   -> error ("Incorrect constructor: \"" ++ show ctor ++ "\" for attribute fmtsRoot")

--------------------------------------------------------
----------
-------------- Others
----------
--------------------------------------------------------

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


narrowFrame :: Int -> T_FRAME -> T_FRAME
narrowFrame i (F s l) = F (s-i) (l-i)

narrowll :: Int -> T_FRAME -> T_FRAME
narrowll i (F s l) = F s (l-i)

besideHeight :: Int -> Int -> Int
besideHeight lh rh
  = if lh == 0 || rh == 0 then 0 else 1


consHeight :: Int -> Int -> Bool -> Int
consHeight pPh acth avail
  | acth == 0  = if pPh > 0 then 1 else 0
  | otherwise  = acth + if avail then 0 else 1

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

eqSetFmtsFill :: Formats -> T_FORMATS
eqSetFmtsFill = AFormat

setFmtsFillblock :: Int -> Formats -> Int -> T_FORMATS
setFmtsFillblock int fmt w
  | int < 0     = afmtTxt "<Error: negative page width in fillblock>"
  | int > w     = afmtTxt (asts int)
  | otherwise   = AFormat fmt

setFmtsApply :: T_ERROR -> T_FORMATS -> T_FORMATS -> T_FORMATS
setFmtsApply True  a  _  =  a
setFmtsApply False _  b  =  b

eqSetFmtsApply :: T_ERROR -> String -> T_FORMATS -> T_FORMATS
eqSetFmtsApply err msg = setFmtsApply err (AFormat (textFmts msg))

afmtTxt :: String -> T_FORMATS
afmtTxt = AFormat . textFmts

asts :: Int -> String
asts 0 = ""
asts 1 = "*"
asts s = ':' : replicate (s-2) '*' ++ ">"

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