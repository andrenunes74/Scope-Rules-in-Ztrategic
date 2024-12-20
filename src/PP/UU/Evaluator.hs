module Examples.AG.PP.UU.Evaluator where

import Examples.AG.PP.UU.Types
import Examples.AG.PP.UU.Core


-- ...................................................................
-- ..... Generated code from Pretty.ag ...............................

narrow_frame i  (F s l) = F (s - i)  (l - i)
narrow_ll    i  (F s l) = F s        (l - i)

set_var_apply cond va vb = if cond then va else vb

beside_height lh rh
  = lh + rh - if (lh == 0 || rh == 0) then 0 else 1

cons_height pPh acth avail
  | acth == 0  = if pPh > 0 then 1 else 0
  | otherwise  = acth + if avail then 0 else 1

afmt_txt = AFormat . text_fmts

set_fmts_empty = AFormat empty_fmts

set_fmts_text string minw error
  = afmt_txt string
  --(if error then (asts minw) else string)

set_fmts_indent int fmts pw minw frame error
  | int < 0    = afmt_txt "<Error: negative indentation>"
 -- int > pw   = afmt_txt . asts $ minw
  | error      = set_fmts_indent' error_indent
  | otherwise  = set_fmts_indent' (indent_fmts frame)
  where set_fmts_indent' fmt_fc
          = case fmts of
              AFormat fs -> AFormat (fmt_fc int fs)
              TFormats as bs ae be
                         -> TFormats (fmt_fc int as)
                                     (fmt_fc int bs) ae be

set_fmts_beside ls rs lh rh frame err
  = set_fmts_ab ls rs set_fmts_beside' "<Error: can't beside two pairs>"
  where set_fmts_beside' as bs
          = set_ab (lh == 0) (rh == 0) as bs
               (if err then error_beside
                       else beside_fmts frame)

set_fmts_above us ls uh lh
  = set_fmts_ab us ls set_fmts_above' "<Error: can't above two pairs>"
  where set_fmts_above' as bs = set_ab (uh == 0) (lh == 0) as bs above_fmts

set_ab aempty bempty as bs fmt_fc
  = if aempty       {- left operand empty?  -}
    then bs
    else if bempty  {- right operand empty? -}
         then as
         else fmt_fc as bs

set_fmts_ab fs gs fmt_fc etxt
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
                         otherwise     -> ( afmt_txt etxt, True )

sem_fmts_dup afs bfs ae be minw
  = {-if (ae && be)
    then afmt_txt . asts $ minw
    else-}
         let get_fmts fs
               = case fs of
                   AFormat as       -> as
                   TFormats _ _ _ _ -> text_fmts "<Error: can't dup a dup>"
             afmts = get_fmts afs
             bfmts = get_fmts bfs
         in  TFormats afmts bfmts ae be

set_fmts_join    (TFormats as bs ae be)  err
  = ( AFormat $ if be
                then (if null as then bs else as)
                else if ae
                     then (if null bs then as else bs)
                     else merge as bs
    , False
    )
set_fmts_join fs@(AFormat _) err
  = if err then (fs, err)
           else (afmt_txt "<Error: can't join a single result>", True)

set_fmts_apply True  a  _  =  a
set_fmts_apply False _  b  =  b

set_fmts_fillblock int fmts
  | int < 0     = afmt_txt "<Error: negative page width in fillblock>"
  | otherwise   = AFormat fmts

set_error_msg numpars len
  = "<Error: incorrect apply expression. #pars "
  ++ show numpars ++ " /= #args "
  ++ show len     ++ ">"
{-
asts 0 = ""
asts 1 = "*"
asts s = '<' : replicate (s-2) '*' ++ ">"
-}
sem_fmts_cdup afs bfs ae be an bn minw em
  = if an /= bn then afmt_txt em
                else sem_fmts_dup afs bfs ae be minw

set_error_msg' apars bpars
  =  "<Error: incorrect choice expression. #pars left " ++ show apars
  ++ " /= #pars right " ++ show bpars
  ++ ">"

set_fmts_filllist ifmts nfmts ih nh frame avail
  = case nfmts of
      AFormat ns -> if ih == 0                       {- left operand empty?   -}
                    then (ns, False)
                    else if nh == 0                  {- right operand empty?  -}
                         then (ifmts, False)
                         else if nh <= 1
                              then ( choose_ab (beside_fmts frame) ifmts ns, False )
                              else ( choose_ab error_beside
                                       ifmts (text_fmts "<Error: element in fill higher than 1>")
                                   , True )
      otherwise  -> ( set_fmts_filllist' . text_fmts $ "<Error: element in fill list is a pair>"
                    , True )
  where set_fmts_filllist' fs
          = set_ab (ih == 0) (nh == 0) fs ifmts (choose_ab error_beside)
        choose_ab bsd_fc = if avail then bsd_fc else above_fmts

set_fmts_render pw fs
  = if pw < 0
    then text_fmts "<Error: negative page width >"
    else case fs of
           AFormat fmts -> fmts
           otherwise    -> text_fmts "<Error: can't render a pair>"



set_fmts_filt (AFormat  fs     ) minw
  = {-if null height1 then ( afmt_txt . asts $ minw , True  )
                    else-} ( AFormat height1        , False )
  where height1 = takeWhile ((<=1).height) fs
set_fmts_filt _ _
  = ( afmt_txt $ "<Error: can not filter a pair>", True )

set_fmts_inv fs
  = case fs of
      AFormat fmts         -> AFormat . set_inv $ fmts
      TFormats as bs ae be -> TFormats (set_inv as) (set_inv bs) ae be
  where set_inv = (:[]) . (Elem 1 0 0) . txtstr . head



vapp fmts spaces pPS frame
  = sem_PPS_Above (\frame -> fmts) (sem_PPS_Indent spaces pPS) frame




---------------------- PPS -------------------------
-- semantic domains
type T_PPS =  T_Frame ->(T_Formats,T_Error,T_PH,T_PLL,T_PW)
-- funcs
sem_PPS_Empty :: T_PPS
sem_PPS_Empty lhs_frame =  ( (set_fmts_empty), False, 0, (0), (0) )
sem_PPS_Text ::String -> T_PPS
sem_PPS_Text string lhs_frame
 = let{ minw = (length string)
   ;    error = (minw > pw)
   ;    f@(F pw _ ) = (lhs_frame)
   }in  ( (set_fmts_text string minw error), error, (1), (minw), minw )
sem_PPS_Indent ::Int -> T_PPS -> T_PPS
sem_PPS_Indent int pPS lhs_frame
 = let{ ( pPS_fmts, pPS_error, pPS_maxh, pPS_minll, pPS_minw )  = pPS (narrow_frame int lhs_frame)
   ;    minw = (int + pPS_minw)
   ;    f@(F pw _ ) = (lhs_frame)
   }in  ( (set_fmts_indent int pPS_fmts pw minw lhs_frame pPS_error)
        , (or [int < 0, int > pw, pPS_error])
        , pPS_maxh
        , (int + pPS_minll)
        , (minw)
        )
sem_PPS_Beside :: T_PPS -> T_PPS -> T_PPS
sem_PPS_Beside left right lhs_frame
 = let{ ( left_fmts, left_error, left_maxh, left_minll, left_minw )  = left (narrow_ll right_minw lhs_frame)
   ;    ( right_fmts, right_error, right_maxh, right_minll, right_minw )  = right (narrow_frame left_minll lhs_frame)
   ;    error = (left_error || right_error)
   ;    fe@(bfmts,berror) = (set_fmts_beside left_fmts right_fmts left_maxh right_maxh lhs_frame error)
   }in  ( (bfmts)
        , (error || berror)
        , (beside_height left_maxh right_maxh)
        , (left_minll + right_minll)
        , (left_minw `max` (left_minll + right_minw))
        )
sem_PPS_Above :: T_PPS -> T_PPS -> T_PPS
sem_PPS_Above upper lower lhs_frame
 = let{ ( upper_fmts, upper_error, upper_maxh, upper_minll, upper_minw )  = upper lhs_frame
   ;    ( lower_fmts, lower_error, lower_maxh, lower_minll, lower_minw )  = lower lhs_frame
   ;    fe@(afmts,aerror) = (set_fmts_above upper_fmts lower_fmts upper_maxh lower_maxh)
   }in  ( (afmts)
        , (or [lower_error, upper_error, aerror])
        , upper_maxh + lower_maxh
        , (lower_minll)
        , (upper_minw `max` lower_minw)
        )
sem_PPS_Dup :: T_PPS -> T_PPS -> T_PPS
sem_PPS_Dup opta optb lhs_frame
 = let{ ( opta_fmts, opta_error, opta_maxh, opta_minll, opta_minw )  = opta lhs_frame
   ;    ( optb_fmts, optb_error, optb_maxh, optb_minll, optb_minw )  = optb lhs_frame
   ;    minw = (opta_minw `min` optb_minw)
   ;    error = (opta_error && optb_error)
   }in  ( (sem_fmts_dup opta_fmts optb_fmts opta_error optb_error minw)
        , (error)
        , (opta_maxh `max` optb_maxh)
        , (opta_minll `min` optb_minll)
        , (minw)
        )
sem_PPS_Join :: T_PPS -> T_PPS
sem_PPS_Join pPS lhs_frame
 = let{ ( pPS_fmts, pPS_error, pPS_maxh, pPS_minll, pPS_minw )  = pPS lhs_frame
   ;    fe@(jfmts,jerror) = (set_fmts_join pPS_fmts pPS_error)
   }in  ( (jfmts), (pPS_error || jerror), pPS_maxh, pPS_minll, pPS_minw )
sem_PPS_Apply :: T_PPC -> T_PPSArgs -> T_PPS
sem_PPS_Apply pPC pPSArgs lhs_frame
 = let{ ( pPC_fmts, pPC_error, pPC_maxh, pPC_reqs, pPC_minll, pPC_minw, pPC_numpars )
         = pPC (pPSArgs_error) (pPSArgs_fmts) lhs_frame (pPSArgs_mins)
   ;    ( pPSArgs_error, pPSArgs_fmts, pPSArgs_mins, pPSArgs_len )  = pPSArgs pPC_reqs
   ;    error = (set_var_apply error_cond True pPC_error)
   ;    error_cond = (pPC_numpars /= pPSArgs_len)
   ;    lem = (length error_msg)
   ;    error_msg = (set_error_msg pPC_numpars pPSArgs_len)
   }in  ( (set_fmts_apply error_cond (AFormat . text_fmts $ error_msg) pPC_fmts)
        , (error)
        , (set_var_apply error_cond 1 pPC_maxh)
        , (set_var_apply error_cond lem pPC_minll)
        , (set_var_apply error_cond lem pPC_minw)
        )
sem_PPS_Fill :: T_FillList -> T_PPS
sem_PPS_Fill fillList lhs_frame
 = let{ ( fillList_fmts, fillList_error, fillList_maxh, fillList_minw, fillList_minll )
         = fillList (empty_fmts) (False) (0) (0) (0) (F w w) (w)
   ;    f@(F w _ ) = (lhs_frame)
   }in  ( (AFormat fillList_fmts), fillList_error, fillList_maxh, fillList_minll, fillList_minw )
sem_PPS_FillBlock ::Int -> T_FillList -> T_PPS
sem_PPS_FillBlock int fillList lhs_frame
 = let{ ( fillList_fmts, fillList_error, fillList_maxh, fillList_minw, fillList_minll )
         = fillList (empty_fmts) (False) (0) (0) (0) (f_frame) (f_width)
   ;    f@(F w _ ) = (lhs_frame)
   ;    f_width = (if int > w then w else int)
   ;    f_frame = (if int > w then lhs_frame else (F int int))
   ;    error = (or [int < 0, fillList_error])
   }in  ( (set_fmts_fillblock int fillList_fmts), (error), fillList_maxh, fillList_minll, fillList_minw )
sem_PPS_Filt :: T_PPS -> T_PPS
sem_PPS_Filt pPS lhs_frame
 = let{ ( pPS_fmts, pPS_error, pPS_maxh, pPS_minll, pPS_minw )  = pPS lhs_frame
   ;    ef@(fmts,error) = (set_fmts_filt pPS_fmts pPS_minw)
   }in  ( (fmts), (error || pPS_error), pPS_maxh, pPS_minll, pPS_minw )
sem_PPS_Inv :: T_PPS -> T_PPS
sem_PPS_Inv pPS lhs_frame
 = let{ ( pPS_fmts, pPS_error, pPS_maxh, pPS_minll, pPS_minw )  = pPS (F maxBound maxBound)
   }in  ( (set_fmts_inv pPS_fmts), (False), (1), (0), (0) )
sem_PPS_Center :: T_CenterList -> T_PPS
sem_PPS_Center centerList lhs_frame
 = let{ ( centerList_maxw, centerList_fmts )  = centerList (centerList_maxw) (sem_PPS_Empty lhs_frame) lhs_frame
   ;    clf@(fmts,error,maxh,minll,minw) = (centerList_fmts)
   }in  ( (fmts), (error), (maxh), (minll), (minw) )
---------------------- PPC -------------------------
-- semantic domains
type T_PPC =  T_Errs -> T_Fmts -> T_Frame -> T_Mins ->
              (T_Formats,T_Error,T_PH,T_Reqs,T_PLL
              ,T_PW,Int)
-- funcs
sem_PPC_Indent ::Int -> T_PPC -> T_PPC
sem_PPC_Indent int pPC lhs_fillerrs lhs_fillfmts lhs_frame lhs_fillmins
 = let{ ( pPC_fmts, pPC_error, pPC_maxh, pPC_reqs, pPC_minll, pPC_minw, pPC_numpars )
         = pPC lhs_fillerrs lhs_fillfmts (narrow_frame int lhs_frame) lhs_fillmins
   ;    minw = (int + pPC_minw)
   ;    f@(F pw _ ) = (lhs_frame)
   }in  ( (set_fmts_indent int pPC_fmts pw minw lhs_frame pPC_error)
        , (or [int < 0, int > pw, pPC_error])
        , pPC_maxh
        , pPC_reqs
        , (int + pPC_minll)
        , (minw)
        , pPC_numpars
        )
sem_PPC_Beside :: T_PPC -> T_PPC -> T_PPC
sem_PPC_Beside left right lhs_fillerrs lhs_fillfmts lhs_frame lhs_fillmins
 = let{ ( left_fmts, left_error, left_maxh, left_reqs, left_minll, left_minw, left_numpars )
         = left (les) (lfs) (narrow_ll right_minw lhs_frame) (lim)
   ;    ( right_fmts, right_error, right_maxh, right_reqs, right_minll, right_minw, right_numpars )
         = right (res) (rfs) (narrow_frame left_minll lhs_frame) (rim)
   ;    i@(lim,rim) = (splitAt left_numpars lhs_fillmins)
   ;    e@(les,res) = (splitAt left_numpars lhs_fillerrs)
   ;    m@(lfs,rfs) = (splitAt left_numpars lhs_fillfmts)
   ;    error = (left_error || right_error)
   ;    fe@(bfmts,berror) = (set_fmts_beside left_fmts right_fmts left_maxh right_maxh lhs_frame error)
   }in  ( (bfmts)
        , (error || berror)
        , (beside_height left_maxh right_maxh)
        , left_reqs ++ right_reqs
        , (left_minll + right_minll)
        , (left_minw `max` (left_minll + right_minw))
        , left_numpars + right_numpars
        )
sem_PPC_Above :: T_PPC -> T_PPC -> T_PPC
sem_PPC_Above upper lower lhs_fillerrs lhs_fillfmts lhs_frame lhs_fillmins
 = let{ ( upper_fmts, upper_error, upper_maxh, upper_reqs, upper_minll, upper_minw, upper_numpars )
         = upper (ues) (ufs) lhs_frame (uim)
   ;    ( lower_fmts, lower_error, lower_maxh, lower_reqs, lower_minll, lower_minw, lower_numpars )
         = lower (les) (lfs) lhs_frame (lim)
   ;    i@(uim,lim) = (splitAt upper_numpars lhs_fillmins)
   ;    e@(ues,les) = (splitAt upper_numpars lhs_fillerrs)
   ;    m@(ufs,lfs) = (splitAt upper_numpars lhs_fillfmts)
   ;    fe@(afmts,aerror) = (set_fmts_above upper_fmts lower_fmts upper_maxh lower_maxh)
   }in  ( (afmts)
        , (or [lower_error, upper_error, aerror])
        , (upper_maxh + lower_maxh)
        , upper_reqs ++ lower_reqs
        , lower_minll
        , (upper_minw `max` lower_minw)
        , upper_numpars + lower_numpars
        )
sem_PPC_Dup :: T_PPC -> T_PPC -> T_PPC
sem_PPC_Dup opta optb lhs_fillerrs lhs_fillfmts lhs_frame lhs_fillmins
 = let{ ( opta_fmts, opta_error, opta_maxh, opta_reqs, opta_minll, opta_minw, opta_numpars )
         = opta lhs_fillerrs lhs_fillfmts lhs_frame lhs_fillmins
   ;    ( optb_fmts, optb_error, optb_maxh, optb_reqs, optb_minll, optb_minw, optb_numpars )
         = optb lhs_fillerrs lhs_fillfmts lhs_frame lhs_fillmins
   ;    minw = (opta_minw `min` optb_minw)
   ;    error = (or [opta_numpars /= optb_numpars, opta_error && optb_error])
   ;    error_msg = (set_error_msg' opta_numpars optb_numpars)
   }in  ( (sem_fmts_cdup opta_fmts optb_fmts opta_error optb_error opta_numpars optb_numpars minw error_msg)
        , (error)
        , (opta_maxh `max` optb_maxh)
        , (zipWith max opta_reqs optb_reqs)
        , (opta_minll `min` optb_minll)
        , (minw)
        , (opta_numpars)
        )
sem_PPC_Join :: T_PPC -> T_PPC
sem_PPC_Join pPC lhs_fillerrs lhs_fillfmts lhs_frame lhs_fillmins
 = let{ ( pPC_fmts, pPC_error, pPC_maxh, pPC_reqs, pPC_minll, pPC_minw, pPC_numpars )
         = pPC lhs_fillerrs lhs_fillfmts lhs_frame lhs_fillmins
   ;    fe@(jfmts,jerror) = (set_fmts_join pPC_fmts pPC_error)
   }in  ( (jfmts), (pPC_error || jerror), pPC_maxh, pPC_reqs, pPC_minll, pPC_minw, pPC_numpars )
sem_PPC_Par :: T_PPC
sem_PPC_Par lhs_fillerrs lhs_fillfmts lhs_frame lhs_fillmins
 = let{ m@(minw,minll,maxh) = (head lhs_fillmins)
   ;    error = (head lhs_fillerrs)
   ;    fmts = (head lhs_fillfmts)
   }in  ( fmts, error, maxh, ([lhs_frame]), minll, minw, 1 )
sem_PPC_Apply :: T_PPC -> T_PPCArgs -> T_PPC
sem_PPC_Apply pPC pPCArgs lhs_fillerrs lhs_fillfmts lhs_frame lhs_fillmins
 = let{ ( pPC_fmts, pPC_error, pPC_maxh, pPC_reqs, pPC_minll, pPC_minw, pPC_numpars )
         = pPC (pPCArgs_error) (pPCArgs_fmts) (lhs_frame) (pPCArgs_ofillmins)
   ;    ( pPCArgs_error, pPCArgs_fmts, pPCArgs_reqs, pPCArgs_ofillmins, pPCArgs_numpars, pPCArgs_len )
         = pPCArgs (lhs_fillerrs) (lhs_fillfmts) (pPC_reqs) (lhs_fillmins)
   ;    error = (set_var_apply error_cond True pPC_error)
   ;    error_cond = (pPC_numpars /= pPCArgs_len)
   ;    lem = (length error_msg)
   ;    error_msg = (set_error_msg pPC_numpars pPCArgs_len)
   }in  ( (set_fmts_apply error_cond (AFormat . text_fmts $ error_msg) pPC_fmts)
        , (error)
        , (set_var_apply error_cond 1 pPC_maxh)
        , (pPCArgs_reqs)
        , (set_var_apply error_cond lem pPC_minll)
        , (set_var_apply error_cond lem pPC_minw)
        , (pPCArgs_numpars)
        )
sem_PPC_Pps :: T_PPS -> T_PPC
sem_PPC_Pps pPS lhs_fillerrs lhs_fillfmts lhs_frame lhs_fillmins
 = let{ ( pPS_fmts, pPS_error, pPS_maxh, pPS_minll, pPS_minw )  = pPS lhs_frame
   }in  ( pPS_fmts, pPS_error, pPS_maxh, ([]), pPS_minll, pPS_minw, (0) )
sem_PPC_Filt :: T_PPC -> T_PPC
sem_PPC_Filt pPC lhs_fillerrs lhs_fillfmts lhs_frame lhs_fillmins
 = let{ ( pPC_fmts, pPC_error, pPC_maxh, pPC_reqs, pPC_minll, pPC_minw, pPC_numpars )
         = pPC lhs_fillerrs lhs_fillfmts lhs_frame lhs_fillmins
   ;    ef@(fmts,error) = (set_fmts_filt pPC_fmts pPC_minw)
   }in  ( (fmts), (error || pPC_error), pPC_maxh, pPC_reqs, pPC_minll, pPC_minw, pPC_numpars )
---------------------- PPSArgs -------------------------
-- semantic domains
type T_PPSArgs =  T_Reqs ->(T_Errs,T_Fmts,T_Mins,Int)
-- funcs
sem_PPSArgs_Nil :: T_PPSArgs
sem_PPSArgs_Nil lhs_reqs =  ( ([]), ([]), ([]), (0) )
sem_PPSArgs_Cons :: T_PPS -> T_PPSArgs -> T_PPSArgs
sem_PPSArgs_Cons pPS pPSArgs lhs_reqs
 = let{ ( pPS_fmts, pPS_error, pPS_maxh, pPS_minll, pPS_minw )  = pPS (head lhs_reqs)
   ;    ( pPSArgs_error, pPSArgs_fmts, pPSArgs_mins, pPSArgs_len )  = pPSArgs (tail lhs_reqs)
   }in  ( (pPS_error:pPSArgs_error), (pPS_fmts:pPSArgs_fmts), ((pPS_minw ,pPS_minll, pPS_maxh):pPSArgs_mins), (pPSArgs_len + 1) )
---------------------- PPCArgs -------------------------
-- semantic domains
type T_PPCArgs =  T_Errs -> T_Fmts -> T_Reqs -> T_Mins ->(T_Errs,T_Fmts,T_Reqs,T_Mins,Int,Int)
-- funcs
sem_PPCArgs_Nil :: T_PPCArgs
sem_PPCArgs_Nil lhs_ifillerrs lhs_ifillfmts lhs_ireqs lhs_ifillmins =  ( ([]), ([]), [], ([]), 0, (0) )
sem_PPCArgs_Cons :: T_PPC -> T_PPCArgs -> T_PPCArgs
sem_PPCArgs_Cons pPC pPCArgs lhs_ifillerrs lhs_ifillfmts lhs_ireqs lhs_ifillmins
 = let{ ( pPC_fmts, pPC_error, pPC_maxh, pPC_reqs, pPC_minll, pPC_minw, pPC_numpars )  = pPC (pef) (pff) (head lhs_ireqs) (pim)
   ;    ( pPCArgs_error, pPCArgs_fmts, pPCArgs_reqs, pPCArgs_ofillmins, pPCArgs_numpars, pPCArgs_len )
         = pPCArgs (lef) (lff) (tail lhs_ireqs) (lim)
   ;    i@(pim,lim) = (splitAt pPC_numpars lhs_ifillmins)
   ;    e@(pef,lef) = (splitAt pPC_numpars lhs_ifillerrs)
   ;    m@(pff,lff) = (splitAt pPC_numpars lhs_ifillfmts)
   }in  ( (pPC_error:pPCArgs_error)
        , (pPC_fmts:pPCArgs_fmts)
        , pPC_reqs ++ pPCArgs_reqs
        , ((pPC_minw ,pPC_minll,pPC_maxh):pPCArgs_ofillmins)
        , pPC_numpars + pPCArgs_numpars
        , (pPCArgs_len + 1)
        )
---------------------- FillList -------------------------
-- semantic domains
type T_FillList =  Formats -> T_Error -> T_PH -> T_PW -> T_PLL -> T_Frame -> T_PW ->(Formats,T_Error,T_PH,T_PW,T_PLL)
-- funcs
sem_FillList_Nil :: T_FillList
sem_FillList_Nil lhs_fmts lhs_error lhs_maxh lhs_minw lhs_minll lhs_frame lhs_pw
 =  ( lhs_fmts, lhs_error, lhs_maxh, lhs_minw, lhs_minll )
sem_FillList_Cons :: T_PPS -> T_FillList -> T_FillList
sem_FillList_Cons pPS fillList lhs_fmts lhs_error lhs_maxh lhs_minw lhs_minll lhs_frame lhs_pw
 = let{ ( pPS_fmts, pPS_error, pPS_maxh, pPS_minll, pPS_minw )  = pPS (lhs_frame)
   ;    ( fillList_fmts, fillList_error, fillList_maxh, fillList_minw, fillList_minll )
         = fillList (ffmts)
                    (lhs_error || ferror)
                    (cons_height pPS_maxh lhs_maxh avail)
                    (if (not avail) || (lhs_minw == lhs_pw) then lhs_pw else lhs_minll)
                    (if ferror then lhs_pw + 1 else if avail then newll else pPS_minw)
                    lhs_frame
                    lhs_pw
   ;    avail = (lhs_pw - newll >= 0)
   ;    newll = (lhs_minll + pPS_minw)
   ;    fe@(ffmts,ferror) = (set_fmts_filllist lhs_fmts pPS_fmts lhs_maxh pPS_maxh lhs_frame avail)
   }in  ( fillList_fmts, (fillList_error || pPS_error), fillList_maxh, fillList_minw, fillList_minll )
---------------------- Root -------------------------
-- semantic domains
type T_Root =  T_PW ->String
-- funcs
sem_Root_Best :: T_PPS -> T_Root
sem_Root_Best pPS lhs_pw
 = let{ ( pPS_fmts, pPS_error, pPS_maxh, pPS_minll, pPS_minw )  = pPS (F lhs_pw lhs_pw)
   }in  (best . set_fmts_render lhs_pw $ pPS_fmts)
sem_Root_All :: T_PPS -> T_Root
sem_Root_All pPS lhs_pw
 = let{ ( pPS_fmts, pPS_error, pPS_maxh, pPS_minll, pPS_minw )  = pPS (F lhs_pw lhs_pw)
   }in  (allf . set_fmts_render lhs_pw $ pPS_fmts)
---------------------- Disp -------------------------
-- semantic domains
type T_Disp =  T_PW ->ShowS
-- funcs
sem_Disp_Disp :: T_PPS -> T_Disp
sem_Disp_Disp pPS lhs_pw
 = let{ ( pPS_fmts, pPS_error, pPS_maxh, pPS_minll, pPS_minw )  = pPS (F lhs_pw lhs_pw)
   }in  (dispf . set_fmts_render lhs_pw $ pPS_fmts)
---------------------- LiftS -------------------------
-- semantic domains
type T_LiftS =  T_Function -> T_Frame ->(T_Formats,T_Error,T_PH,T_PLL,T_PW)
-- funcs
sem_LiftS_Lift :: T_PPS -> T_LiftS
sem_LiftS_Lift pPS lhs_f lhs_frame
 = let{ ( pPS_fmts, pPS_error, pPS_maxh, pPS_minll, pPS_minw )  = pPS lhs_frame
   }in  ( (lhs_f pPS_fmts), pPS_error, pPS_maxh, pPS_minll, pPS_minw )
---------------------- LiftC -------------------------
-- funcs
sem_LiftC_Lift pPC lhs_f lhs_fillerrs lhs_fillfmts lhs_frame lhs_fillmins
 = let{ ( pPC_fmts, pPC_error, pPC_maxh, pPC_reqs, pPC_minll, pPC_minw, pPC_numpars )
         = pPC lhs_fillerrs lhs_fillfmts lhs_frame lhs_fillmins
   }in  ( (lhs_f pPC_fmts), pPC_error, pPC_maxh, pPC_reqs, pPC_minll, pPC_minw, pPC_numpars )
sem_LiftC_Pair pPC lhs_f lhs_fillerrs lhs_fillfmts lhs_frame lhs_fillmins
 = let{ ( pPC_fmts, pPC_error, pPC_maxh, pPC_reqs, pPC_minll, pPC_minw, pPC_numpars )
         = pPC lhs_fillerrs lhs_fillfmts lhs_frame lhs_fillmins
   ;    fe@(fmts,error) = (lhs_f pPC_fmts)
   }in  ( (fmts), (pPC_error || error), pPC_maxh, pPC_reqs, pPC_minll, pPC_minw, pPC_numpars )
---------------------- CenterList -------------------------
-- semantic domains
type T_CenterList =  Int -> T_SynPPS -> T_Frame ->(Int,T_SynPPS)
-- funcs
sem_CenterList_Nil :: T_CenterList
sem_CenterList_Nil lhs_maxw lhs_fmts lhs_frame =  ( (0), lhs_fmts )
sem_CenterList_Cons :: T_PPS -> T_CenterList -> T_CenterList
sem_CenterList_Cons pPS centerList lhs_maxw lhs_fmts lhs_frame
 = let{ ( pPS_fmts, pPS_error, pPS_maxh, pPS_minll, pPS_minw )  = pPS (lhs_frame)
   ;    ( centerList_maxw, centerList_fmts )  = centerList lhs_maxw (vapp lhs_fmts spaces pPS lhs_frame) lhs_frame
   ;    spaces = ((lhs_maxw - pPS_minw) `div` 2)
   }in  ( (pPS_minw `max` centerList_maxw), centerList_fmts )