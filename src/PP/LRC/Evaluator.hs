module Examples.AG.PP.LRC.Evaluator where

import Examples.AG.PP.LRC.LrcPrelude
import Examples.AG.PP.LRC.Types
import Examples.AG.PP.LRC.Core

-- -------------------------------------------------------------------
-- Interface with the strict evaluator -------------------------------


sem_PPS_Empty     = lambda_Empty_1
sem_PPS_Text      = lambda_Text_1
sem_PPS_Indent    = lambda_Indent_1
sem_PPS_Beside    = lambda_Beside_1
sem_PPS_Above  a b   = lambda_Above_1 a b
sem_FillList_Cons = lambda_ConsFillList_1
sem_FillList_Nil  = lambda_NilFillList_1
sem_PPS_Fill      = lambda_Fill_1
sem_PPS_FillBlock = lambda_FillBlock_1
sem_PPS_Dup a b   = lambda_Dup_1 a b
sem_PPS_Join      = lambda_Join_1


sem_PPC_Indent a b  = lambda_IndentC_1 a b
sem_PPC_Beside a b  = lambda_BesideC_1 a b
sem_PPC_Above a b   = lambda_AboveC_1 a b
sem_PPC_Dup a b     = lambda_DupC_1 a b
sem_PPC_Join        = lambda_JoinC_1
sem_PPC_Par         = lambda_ParC_1

sem_PPSArgs_Cons    = lambda_ConsArgs_1
sem_PPSArgs_Nil     = lambda_NilArgs_1
sem_PPS_Apply a b   = lambda_Apply_1 a b

sem_PPCArgs_Nil   = lambda_NilPPCArgs_1
sem_PPCArgs_Cons  = lambda_ConsPPCArgs_1
sem_PPC_Apply a b = lambda_ApplyC_1 a b 

sem_Root_Best     = lambda_Best_1
sem_Root_All      = lambda_All_1

-- Saraiva 2021!
sem_Disp_Disp     = lambda_Disp_1



--
-- Deforestated Visit-Functions
--
lambda_All_1 t_PPS_1 x_pw = (x_fmts)
  where
      x_frame_1 = (C_F_1 x_pw x_pw)
      (t_PPS_2,x_maxh_1 , x_minll_1 , x_minw_1) = t_PPS_1
      (x_error_1 , x_fmts_1) = t_PPS_2 x_frame_1
      x_fmts = (eq_all_fmts x_pw x_fmts_1)
lambda_Best_1 t_PPS_1 x_pw = (x_fmts)
  where
      x_frame_1 = (C_F_1 x_pw x_pw)
      (t_PPS_2,x_maxh_1 , x_minll_1 , x_minw_1) = t_PPS_1
      (x_error_1 , x_fmts_1) = t_PPS_2 x_frame_1
      x_fmts = (eq_best_fmts x_pw x_fmts_1)

-- Saraiva 2021!
lambda_Disp_1 t_PPS_1 x_pw = (x_fmts)
  where
      x_frame_1 = (C_F_1 x_pw x_pw)
      (t_PPS_2,x_maxh_1 , x_minll_1 , x_minw_1) = t_PPS_1
      (x_error_1 , x_fmts_1) = t_PPS_2 x_frame_1
      x_fmts = (eq_disp_fmts x_pw x_fmts_1)



lambda_Above_1 t_PPS_2_1 t_PPS_3_1 = (lambda_Above_2 x_maxh_1 x_maxh_2 t_PPS_2_2 t_PPS_3_2 , x_maxh , x_minll_2 , x_minw)
  where
      (t_PPS_3_2,x_maxh_2 , x_minll_2 , x_minw_2) = t_PPS_3_1
      (t_PPS_2_2,x_maxh_1 , x_minll_1 , x_minw_1) = t_PPS_2_1
      x_maxh = (x_maxh_1+x_maxh_2)
      x_minw = (max x_minw_1 x_minw_2)
lambda_Apply_1 t_PPC_1 t_PPSArgs_1 = (lambda_Apply_2 x_error_cond x_error_msg t_PPC_3 t_PPSArgs_2 , x_maxh , x_minll , x_minw)
  where
      (t_PPSArgs_2,x_len_2 , x_mins_2) = t_PPSArgs_1
      (t_PPC_2,x_numpars_1) = t_PPC_1
      x_error_cond = (x_numpars_1/=x_len_2)
      x_error_msg = (set_error_msg x_numpars_1 x_len_2)
      x_lem = (toInteger (length x_error_msg))
      (t_PPC_3,x_maxh_1 , x_minll_1 , x_minw_1) = t_PPC_2 x_mins_2
      x_maxh = (set_var_apply x_error_cond 1 x_maxh_1)
      x_minll = (set_var_apply x_error_cond x_lem x_minll_1)
      x_minw = (set_var_apply x_error_cond x_lem x_minw_1)
lambda_Beside_1 t_PPS_2_1 t_PPS_3_1 = (lambda_Beside_2 x_maxh_1 x_minll_1 x_maxh_2 x_minw_2 t_PPS_2_2 t_PPS_3_2 , x_maxh , x_minll , x_minw)
  where
      (t_PPS_3_2,x_maxh_2 , x_minll_2 , x_minw_2) = t_PPS_3_1
      (t_PPS_2_2,x_maxh_1 , x_minll_1 , x_minw_1) = t_PPS_2_1
      x_maxh = (beside_height x_maxh_1 x_maxh_2)
      x_minll = (x_minll_1+x_minll_2)
      x_minw = (max x_minw_1 (x_minll_1+x_minw_2))
lambda_Dup_1 t_PPS_2_1 t_PPS_3_1 = (lambda_Dup_2 x_l_minw t_PPS_2_2 t_PPS_3_2 , x_maxh , x_minll , x_l_minw)
  where
      (t_PPS_3_2,x_maxh_2 , x_minll_2 , x_minw_2) = t_PPS_3_1
      (t_PPS_2_2,x_maxh_1 , x_minll_1 , x_minw_1) = t_PPS_2_1
      x_maxh = (max x_maxh_1 x_maxh_2)
      x_minll = (min x_minll_1 x_minll_2)
      x_l_minw = (min x_minw_1 x_minw_2)
lambda_Empty_1 = (lambda_Empty_2 , x_maxh , x_minll , x_minw)
  where
      x_minw = 0
      x_minll = 0
      x_maxh = 0
lambda_Fill_1 t_FillList_1 = (lambda_Fill_2 t_FillList_2 , x_maxh_1 , x_minll_1 , x_minw_1)
  where
      x_minwi_1 = 0
      x_minlli_1 = 0
      x_maxhi_1 = 0
      (t_FillList_2,x_maxh_1 , x_minll_1 , x_minw_1) = t_FillList_1 x_minlli_1 x_minwi_1 x_maxhi_1
lambda_FillBlock_1 t_INT_1 t_FillList_1 = (lambda_FillBlock_2 t_INT_1 t_FillList_2 , x_maxh_2 , x_minll_2 , x_minw_2)
  where
      x_minwi_2 = 0
      x_minlli_2 = 0
      x_maxhi_2 = 0
      (t_FillList_2,x_maxh_2 , x_minll_2 , x_minw_2) = t_FillList_1 x_minlli_2 x_minwi_2 x_maxhi_2
lambda_Indent_1 t_INT_1 t_PPS_2_1 = (lambda_Indent_2 x_l_minw t_INT_1 t_PPS_2_2 , x_maxh_2 , x_minll , x_l_minw)
  where
      (t_PPS_2_2,x_maxh_2 , x_minll_2 , x_minw_2) = t_PPS_2_1
      x_l_minw = (t_INT_1+x_minw_2)
      x_minll = (t_INT_1+x_minll_2)
lambda_Join_1 t_PPS_2_1 = (lambda_Join_2 t_PPS_2_2 , x_maxh_1 , x_minll_1 , x_minw_1)
  where
      (t_PPS_2_2,x_maxh_1 , x_minll_1 , x_minw_1) = t_PPS_2_1
lambda_Text_1 t_STR_1 = (lambda_Text_2 x_l_minw t_STR_1 , x_maxh , x_l_minw , x_l_minw)
  where
      x_maxh = 1
      x_l_minw = (toInteger (length t_STR_1))
lambda_Above_2 x_maxh_1 x_maxh_2 t_PPS_2_2 t_PPS_3_2 x_frame = (x_error , x_fmts)
  where
      (x_error_1 , x_fmts_1) = t_PPS_2_2 x_frame
      (x_error_2 , x_fmts_2) = t_PPS_3_2 x_frame
      x_fe = (set_fmts_above x_fmts_1 x_fmts_2 x_maxh_1 x_maxh_2)
      x_aerror = (snd_Pair_T_Formats x_fe)
      x_error = ((x_error_1||x_error_2)||x_aerror)
      x_fmts = (fst_Pair_T_Formats x_fe)
lambda_Apply_2 x_error_cond x_error_msg t_PPC_3 t_PPSArgs_2 x_frame = (x_l_error , x_fmts)
  where
      (t_PPC_4,x_reqs_1) = t_PPC_3 x_frame
      (x_error_2 , x_fmts_2) = t_PPSArgs_2 x_reqs_1
      (x_error_1 , x_fmts_1) = t_PPC_4 x_error_2 x_fmts_2
      x_l_error = (set_var_apply x_error_cond True x_error_1)
      x_fmts = (eq_set_fmts_apply x_error_cond x_error_msg x_fmts_1)
lambda_Beside_2 x_maxh_1 x_minll_1 x_maxh_2 x_minw_2 t_PPS_2_2 t_PPS_3_2 x_frame = (x_error , x_fmts)
  where
      x_frame_1 = (narrow_ll x_minw_2 x_frame)
      (x_error_1 , x_fmts_1) = t_PPS_2_2 x_frame_1
      x_frame_2 = (narrow_frame x_minll_1 x_frame)
      (x_error_2 , x_fmts_2) = t_PPS_3_2 x_frame_2
      x_l_error = (x_error_1||x_error_2)
      x_fe = (set_fmts_beside x_fmts_1 x_fmts_2 x_maxh_1 x_maxh_2 x_frame x_l_error)
      x_berror = (snd_Pair_T_Formats x_fe)
      x_error = (x_l_error||x_berror)
      x_fmts = (fst_Pair_T_Formats x_fe)
lambda_Dup_2 x_l_minw t_PPS_2_2 t_PPS_3_2 x_frame = (x_l_error , x_fmts)
  where
      (x_error_1 , x_fmts_1) = t_PPS_2_2 x_frame
      (x_error_2 , x_fmts_2) = t_PPS_3_2 x_frame
      x_l_error = (x_error_1 && x_error_2)
      x_fmts = (sem_fmts_dup x_fmts_1 x_fmts_2 x_error_1 x_error_2 x_l_minw)
lambda_Empty_2 x_frame = (x_error , x_fmts)
  where
      x_fmts = (set_fmts_empty )
      x_error = False
lambda_Fill_2 t_FillList_2 x_frame = (x_error_1 , x_fmts)
  where
      x_l_pw = case x_frame of { (C_F_1 w _) -> w }
      x_frame_1 = (C_F_1 x_l_pw x_l_pw)
      x_fmtsi_1 = (empty_fmts )
      x_errori_1 = False
      (x_error_1 , x_fmts_1) = t_FillList_2 x_errori_1 x_fmtsi_1 x_frame_1 x_l_pw
      x_fmts = (eq_set_fmts_fill x_fmts_1)
lambda_FillBlock_2 t_INT_1 t_FillList_2 x_frame = (x_error , x_fmts)
  where
      x_l_pw = case x_frame of { (C_F_1 w _) -> w }
      x_fmtsi_2 = (empty_fmts )
      x_errori_2 = False
      x_frame_2 = (C_F_1 t_INT_1 t_INT_1)
      (x_error_2 , x_fmts_2) = t_FillList_2 x_errori_2 x_fmtsi_2 x_frame_2 t_INT_1
      x_error = (((t_INT_1<0)||(t_INT_1>x_l_pw))||x_error_2)
      x_fmts = (set_fmts_fillblock t_INT_1 x_fmts_2 x_l_pw)
lambda_Indent_2 x_l_minw t_INT_1 t_PPS_2_2 x_frame = (x_error , x_fmts)
  where
      x_pw = case x_frame of { (C_F_1 w _) -> w }
      x_frame_2 = (narrow_frame t_INT_1 x_frame)
      (x_error_2 , x_fmts_2) = t_PPS_2_2 x_frame_2
      x_error = (((t_INT_1<0)||(t_INT_1>x_pw))||x_error_2)
      x_fmts = (set_fmts_indent t_INT_1 x_fmts_2 x_pw x_l_minw x_frame x_error_2)
lambda_Join_2 t_PPS_2_2 x_frame = (x_error , x_fmts)
  where
      (x_error_1 , x_fmts_1) = t_PPS_2_2 x_frame
      x_fe = (set_fmts_join x_fmts_1 x_error_1)
      x_jerror = (snd_Pair_T_Formats x_fe)
      x_error = (x_error_1||x_jerror)
      x_fmts = (fst_Pair_T_Formats x_fe)
lambda_Text_2 x_l_minw t_STR_1 x_frame = (x_l_error , x_fmts)
  where
      x_pw = case x_frame of { (C_F_1 w _) -> w }
      x_l_error = (x_l_minw>x_pw)
      x_fmts = (set_fmts_text t_STR_1 x_l_minw x_l_error)
lambda_AboveC_1 t_PPC_2_1 t_PPC_3_1 = (lambda_AboveC_2 x_numpars_1 t_PPC_2_2 t_PPC_3_2 , x_numpars)
  where
      (t_PPC_3_2,x_numpars_2) = t_PPC_3_1
      (t_PPC_2_2,x_numpars_1) = t_PPC_2_1
      x_numpars = (x_numpars_1+x_numpars_2)
lambda_ApplyC_1 t_PPC_2_1 t_PPCArgs_1 = (lambda_ApplyC_2 x_numpars_2 t_PPC_2_1 t_PPCArgs_2 , x_numpars_2)
  where
      (t_PPCArgs_2,x_numpars_2) = t_PPCArgs_1
lambda_BesideC_1 t_PPC_2_1 t_PPC_3_1 = (lambda_BesideC_2 x_numpars_1 t_PPC_2_2 t_PPC_3_2 , x_numpars)
  where
      (t_PPC_3_2,x_numpars_2) = t_PPC_3_1
      (t_PPC_2_2,x_numpars_1) = t_PPC_2_1
      x_numpars = (x_numpars_1+x_numpars_2)
lambda_DupC_1 t_PPC_2_1 t_PPC_3_1 = (lambda_DupC_2 x_numpars_1 t_PPC_2_2 t_PPC_3_1 , x_numpars_1)
  where
      (t_PPC_2_2,x_numpars_1) = t_PPC_2_1
lambda_IndentC_1 t_INT_1 t_PPC_2_1 = (lambda_IndentC_2 t_INT_1 t_PPC_2_2 , x_numpars_2)
  where
      (t_PPC_2_2,x_numpars_2) = t_PPC_2_1
lambda_JoinC_1 t_PPC_2_1 = (lambda_JoinC_2 t_PPC_2_2 , x_numpars_1)
  where
      (t_PPC_2_2,x_numpars_1) = t_PPC_2_1
lambda_ParC_1 = (lambda_ParC_2 , x_numpars)
  where
      x_numpars = 1
lambda_AboveC_2 x_numpars_1 t_PPC_2_2 t_PPC_3_2 x_fillmins = (lambda_AboveC_3 x_maxh_1 x_numpars_1 x_maxh_2 t_PPC_2_3 t_PPC_3_3 , x_maxh , x_minll_2 , x_minw)
  where
      x_i = (splitAt (toInt x_numpars_1) x_fillmins)
      x_fillmins_1 = (fst x_i)
      (t_PPC_2_3,x_maxh_1 , x_minll_1 , x_minw_1) = t_PPC_2_2 x_fillmins_1
      x_fillmins_2 = (snd x_i)
      (t_PPC_3_3,x_maxh_2 , x_minll_2 , x_minw_2) = t_PPC_3_2 x_fillmins_2
      x_maxh = (x_maxh_1+x_maxh_2)
      x_minw = (max x_minw_1 x_minw_2)
lambda_ApplyC_2 x_numpars_2 t_PPC_2_1 t_PPCArgs_2 x_fillmins = (lambda_ApplyC_3 x_error_cond x_error_msg t_PPC_2_3 t_PPCArgs_3 , x_maxh , x_minll , x_minw)
  where
      (t_PPCArgs_3,x_fillmins_2 , x_len_2) = t_PPCArgs_2 x_fillmins
      x_error_cond = (x_numpars_2/=x_len_2)
      (t_PPC_2_2,x_numpars_1) = t_PPC_2_1
      x_error_msg = (set_error_msg x_numpars_1 x_len_2)
      x_lem = (toInteger (length x_error_msg))
      (t_PPC_2_3,x_maxh_1 , x_minll_1 , x_minw_1) = t_PPC_2_2 x_fillmins_2
      x_maxh = (set_var_apply x_error_cond 1 x_maxh_1)
      x_minll = (set_var_apply x_error_cond x_lem x_minll_1)
      x_minw = (set_var_apply x_error_cond x_lem x_minw_1)
lambda_BesideC_2 x_numpars_1 t_PPC_2_2 t_PPC_3_2 x_fillmins = (lambda_BesideC_3 x_maxh_1 x_minll_1 x_numpars_1 x_maxh_2 x_minw_2 t_PPC_2_3 t_PPC_3_3 , x_maxh , x_minll , x_minw)
  where
      x_i = (splitAt (toInt x_numpars_1) x_fillmins)
      x_fillmins_1 = (fst x_i)
      (t_PPC_2_3,x_maxh_1 , x_minll_1 , x_minw_1) = t_PPC_2_2 x_fillmins_1
      x_fillmins_2 = (snd x_i)
      (t_PPC_3_3,x_maxh_2 , x_minll_2 , x_minw_2) = t_PPC_3_2 x_fillmins_2
      x_maxh = (beside_height x_maxh_1 x_maxh_2)
      x_minll = (x_minll_1+x_minll_2)
      x_minw = (max x_minw_1 (x_minll_1+x_minw_2))
lambda_DupC_2 x_numpars_1 t_PPC_2_2 t_PPC_3_1 x_fillmins = (lambda_DupC_3 x_error_msg x_l_minw x_numpars_1 x_numpars_2 t_PPC_2_3 t_PPC_3_3 , x_maxh , x_minll , x_l_minw)
  where
      (t_PPC_2_3,x_maxh_1 , x_minll_1 , x_minw_1) = t_PPC_2_2 x_fillmins
      (t_PPC_3_2,x_numpars_2) = t_PPC_3_1
      x_error_msg = (eq_set_error_msg x_numpars_1 x_numpars_2)
      (t_PPC_3_3,x_maxh_2 , x_minll_2 , x_minw_2) = t_PPC_3_2 x_fillmins
      x_maxh = (max x_maxh_1 x_maxh_2)
      x_minll = (min x_minll_1 x_minll_2)
      x_l_minw = (min x_minw_1 x_minw_2)
lambda_IndentC_2 t_INT_1 t_PPC_2_2 x_fillmins = (lambda_IndentC_3 x_l_minw t_INT_1 t_PPC_2_3 , x_maxh_2 , x_minll , x_l_minw)
  where
      (t_PPC_2_3,x_maxh_2 , x_minll_2 , x_minw_2) = t_PPC_2_2 x_fillmins
      x_l_minw = (t_INT_1+x_minw_2)
      x_minll = (t_INT_1+x_minll_2)
lambda_JoinC_2 t_PPC_2_2 x_fillmins = (lambda_JoinC_3 t_PPC_2_3 , x_maxh_1 , x_minll_1 , x_minw_1)
  where
      (t_PPC_2_3,x_maxh_1 , x_minll_1 , x_minw_1) = t_PPC_2_2 x_fillmins
lambda_ParC_2 x_fillmins = (lambda_ParC_3 , x_maxh , x_minll , x_minw)
  where
      x_l_m = (head x_fillmins)
      x_maxh = (third_t x_l_m)
      x_minll = (snd_t x_l_m)
      x_minw = (fst_t x_l_m)
lambda_AboveC_3 x_maxh_1 x_numpars_1 x_maxh_2 t_PPC_2_3 t_PPC_3_3 x_frame = (lambda_AboveC_4 x_maxh_1 x_numpars_1 x_maxh_2 t_PPC_2_4 t_PPC_3_4 , x_reqs)
  where
      (t_PPC_2_4,x_reqs_1) = t_PPC_2_3 x_frame
      (t_PPC_3_4,x_reqs_2) = t_PPC_3_3 x_frame
      x_reqs = (x_reqs_1++x_reqs_2)
lambda_ApplyC_3 x_error_cond x_error_msg t_PPC_2_3 t_PPCArgs_3 x_frame = (lambda_ApplyC_4 x_error_cond x_error_msg t_PPC_2_4 t_PPCArgs_4 , x_reqs_2)
  where
      (t_PPC_2_4,x_reqs_1) = t_PPC_2_3 x_frame
      (t_PPCArgs_4,x_reqs_2) = t_PPCArgs_3 x_reqs_1
lambda_BesideC_3 x_maxh_1 x_minll_1 x_numpars_1 x_maxh_2 x_minw_2 t_PPC_2_3 t_PPC_3_3 x_frame = (lambda_BesideC_4 x_frame x_maxh_1 x_numpars_1 x_maxh_2 t_PPC_2_4 t_PPC_3_4 , x_reqs)
  where
      x_frame_1 = (narrow_ll x_minw_2 x_frame)
      (t_PPC_2_4,x_reqs_1) = t_PPC_2_3 x_frame_1
      x_frame_2 = (narrow_frame x_minll_1 x_frame)
      (t_PPC_3_4,x_reqs_2) = t_PPC_3_3 x_frame_2
      x_reqs = (x_reqs_1++x_reqs_2)
lambda_DupC_3 x_error_msg x_l_minw x_numpars_1 x_numpars_2 t_PPC_2_3 t_PPC_3_3 x_frame = (lambda_DupC_4 x_error_msg x_l_minw x_numpars_1 x_numpars_2 t_PPC_2_4 t_PPC_3_4 , x_reqs)
  where
      (t_PPC_2_4,x_reqs_1) = t_PPC_2_3 x_frame
      (t_PPC_3_4,x_reqs_2) = t_PPC_3_3 x_frame
      x_reqs = (eq_DupC x_reqs_1 x_reqs_2)
lambda_IndentC_3 x_l_minw t_INT_1 t_PPC_2_3 x_frame = (lambda_IndentC_4 x_l_minw x_pw t_INT_1 x_frame t_PPC_2_4 , x_reqs_2)
  where
      x_pw = case x_frame of { (C_F_1 w _) -> w }
      x_frame_2 = (narrow_frame t_INT_1 x_frame)
      (t_PPC_2_4,x_reqs_2) = t_PPC_2_3 x_frame_2
lambda_JoinC_3 t_PPC_2_3 x_frame = (lambda_JoinC_4 t_PPC_2_4 , x_reqs_1)
  where
      (t_PPC_2_4,x_reqs_1) = t_PPC_2_3 x_frame
lambda_ParC_3 x_frame = (lambda_ParC_4 , x_reqs)
  where
      x_reqs = ((:) x_frame ([] ))
lambda_AboveC_4 x_maxh_1 x_numpars_1 x_maxh_2 t_PPC_2_4 t_PPC_3_4 x_fillerrs x_fillfmts = (x_error , x_fmts)
  where
      x_e = (splitAt (toInt x_numpars_1) x_fillerrs)
      x_fillerrs_1 = (fst x_e)
      x_fillerrs_2 = (snd x_e)
      x_m = (splitAt (toInt x_numpars_1) x_fillfmts)
      x_fillfmts_1 = (fst x_m)
      (x_error_1 , x_fmts_1) = t_PPC_2_4 x_fillerrs_1 x_fillfmts_1
      x_fillfmts_2 = (snd x_m)
      (x_error_2 , x_fmts_2) = t_PPC_3_4 x_fillerrs_2 x_fillfmts_2
      x_fe = (set_fmts_above x_fmts_1 x_fmts_2 x_maxh_1 x_maxh_2)
      x_aerror = (snd_Pair_T_Formats x_fe)
      x_error = ((x_error_1||x_error_2)||x_aerror)
      x_fmts = (fst_Pair_T_Formats x_fe)
lambda_ApplyC_4 x_error_cond x_error_msg t_PPC_2_4 t_PPCArgs_4 x_fillerrs x_fillfmts = (x_l_error , x_fmts)
  where
      (x_error_2 , x_fmts_2) = t_PPCArgs_4 x_fillerrs x_fillfmts
      (x_error_1 , x_fmts_1) = t_PPC_2_4 x_error_2 x_fmts_2
      x_l_error = (set_var_apply x_error_cond True x_error_1)
      x_fmts = (eq_set_fmts_apply x_error_cond x_error_msg x_fmts_1)
lambda_BesideC_4 x_frame x_maxh_1 x_numpars_1 x_maxh_2 t_PPC_2_4 t_PPC_3_4 x_fillerrs x_fillfmts = (x_error , x_fmts)
  where
      x_e = (splitAt (toInt x_numpars_1) x_fillerrs)
      x_fillerrs_1 = (fst x_e)
      x_fillerrs_2 = (snd x_e)
      x_m = (splitAt (toInt x_numpars_1) x_fillfmts)
      x_fillfmts_1 = (fst x_m)
      (x_error_1 , x_fmts_1) = t_PPC_2_4 x_fillerrs_1 x_fillfmts_1
      x_fillfmts_2 = (snd x_m)
      (x_error_2 , x_fmts_2) = t_PPC_3_4 x_fillerrs_2 x_fillfmts_2
      x_l_error = (x_error_1||x_error_2)
      x_fe = (set_fmts_beside x_fmts_1 x_fmts_2 x_maxh_1 x_maxh_2 x_frame x_l_error)
      x_berror = (snd_Pair_T_Formats x_fe)
      x_error = (x_l_error||x_berror)
      x_fmts = (fst_Pair_T_Formats x_fe)
lambda_DupC_4 x_error_msg x_l_minw x_numpars_1 x_numpars_2 t_PPC_2_4 t_PPC_3_4 x_fillerrs x_fillfmts = (x_l_error , x_fmts)
  where
      (x_error_1 , x_fmts_1) = t_PPC_2_4 x_fillerrs x_fillfmts
      (x_error_2 , x_fmts_2) = t_PPC_3_4 x_fillerrs x_fillfmts
      x_l_error = ((x_numpars_1/=x_numpars_2)||(x_error_1 && x_error_2))
      x_fmts = (sem_fmts_cdup x_fmts_1 x_fmts_2 x_error_1 x_error_2 x_numpars_1 x_numpars_2 x_l_minw x_error_msg)
lambda_IndentC_4 x_l_minw x_pw t_INT_1 x_frame t_PPC_2_4 x_fillerrs x_fillfmts = (x_error , x_fmts)
  where
      (x_error_2 , x_fmts_2) = t_PPC_2_4 x_fillerrs x_fillfmts
      x_error = (((t_INT_1<0)||(t_INT_1>x_pw))||x_error_2)
      x_fmts = (set_fmts_indent t_INT_1 x_fmts_2 x_pw x_l_minw x_frame x_error_2)
lambda_JoinC_4 t_PPC_2_4 x_fillerrs x_fillfmts = (x_error , x_fmts)
  where
      (x_error_1 , x_fmts_1) = t_PPC_2_4 x_fillerrs x_fillfmts
      x_fe = (set_fmts_join x_fmts_1 x_error_1)
      x_jerror = (snd_Pair_T_Formats x_fe)
      x_error = (x_error_1||x_jerror)
      x_fmts = (fst_Pair_T_Formats x_fe)
lambda_ParC_4 x_fillerrs x_fillfmts = (x_l_error , x_fmts)
  where
      x_l_error = (head x_fillerrs)
      x_fmts = (head x_fillfmts)
lambda_ConsPPCArgs_1 t_PPC_1 t_PPCArgs_2_1 = (lambda_ConsPPCArgs_2 x_numpars_1 t_PPC_2 t_PPCArgs_2_2 , x_numpars)
  where
      (t_PPCArgs_2_2,x_numpars_2) = t_PPCArgs_2_1
      (t_PPC_2,x_numpars_1) = t_PPC_1
      x_numpars = (x_numpars_1+x_numpars_2)
lambda_NilPPCArgs_1 = (lambda_NilPPCArgs_2 , x_numpars)
  where
      x_numpars = 0
lambda_ConsPPCArgs_2 x_numpars_1 t_PPC_2 t_PPCArgs_2_2 x_ifillmins = (lambda_ConsPPCArgs_3 x_numpars_1 t_PPC_3 t_PPCArgs_2_3 , x_fillmins , x_len)
  where
      x_i = (splitAt (toInt x_numpars_1) x_ifillmins)
      x_fillmins_1 = (fst x_i)
      (t_PPC_3,x_maxh_1 , x_minll_1 , x_minw_1) = t_PPC_2 x_fillmins_1
      x_ifillmins_2 = (snd x_i)
      (t_PPCArgs_2_3,x_fillmins_2 , x_len_2) = t_PPCArgs_2_2 x_ifillmins_2
      x_fillmins = ((:) (C_Triple_1 x_minw_1 x_minll_1 x_maxh_1) x_fillmins_2)
      x_len = (x_len_2+1)
lambda_NilPPCArgs_2 x_ifillmins = (lambda_NilPPCArgs_3 , x_fillmins , x_len)
  where
      x_len = 0
      x_fillmins = ([] )
lambda_ConsPPCArgs_3 x_numpars_1 t_PPC_3 t_PPCArgs_2_3 x_ireqs = (lambda_ConsPPCArgs_4 x_numpars_1 t_PPC_4 t_PPCArgs_2_4 , x_reqs)
  where
      x_frame_1 = (head x_ireqs)
      (t_PPC_4,x_reqs_1) = t_PPC_3 x_frame_1
      x_ireqs_2 = (tail x_ireqs)
      (t_PPCArgs_2_4,x_reqs_2) = t_PPCArgs_2_3 x_ireqs_2
      x_reqs = (x_reqs_1++x_reqs_2)
lambda_NilPPCArgs_3 x_ireqs = (lambda_NilPPCArgs_4 , x_reqs)
  where
      x_reqs = ([] )
lambda_ConsPPCArgs_4 x_numpars_1 t_PPC_4 t_PPCArgs_2_4 x_ifillerrs x_ifillfmts = (x_error , x_fmts)
  where
      x_e = (splitAt (toInt x_numpars_1) x_ifillerrs)
      x_fillerrs_1 = (fst x_e)
      x_ifillerrs_2 = (snd x_e)
      x_m = (splitAt (toInt x_numpars_1) x_ifillfmts)
      x_fillfmts_1 = (fst x_m)
      (x_error_1 , x_fmts_1) = t_PPC_4 x_fillerrs_1 x_fillfmts_1
      x_ifillfmts_2 = (snd x_m)
      (x_error_2 , x_fmts_2) = t_PPCArgs_2_4 x_ifillerrs_2 x_ifillfmts_2
      x_error = ((:) x_error_1 x_error_2)
      x_fmts = ((:) x_fmts_1 x_fmts_2)
lambda_NilPPCArgs_4 x_ifillerrs x_ifillfmts = (x_error , x_fmts)
  where
      x_fmts = ([] )
      x_error = ([] )
lambda_ConsArgs_1 t_PPS_1 t_PPSArgs_2_1 = (lambda_ConsArgs_2 t_PPS_2 t_PPSArgs_2_2 , x_len , x_mins)
  where
      (t_PPSArgs_2_2,x_len_2 , x_mins_2) = t_PPSArgs_2_1
      x_len = (x_len_2+1)
      (t_PPS_2,x_maxh_1 , x_minll_1 , x_minw_1) = t_PPS_1
      x_mins = ((:) (C_Triple_1 x_minw_1 x_minll_1 x_maxh_1) x_mins_2)
lambda_NilArgs_1 = (lambda_NilArgs_2 , x_len , x_mins)
  where
      x_mins = ([] )
      x_len = 0
lambda_ConsArgs_2 t_PPS_2 t_PPSArgs_2_2 x_reqs = (x_error , x_fmts)
  where
      x_frame_1 = (head x_reqs)
      (x_error_1 , x_fmts_1) = t_PPS_2 x_frame_1
      x_reqs_2 = (tail x_reqs)
      (x_error_2 , x_fmts_2) = t_PPSArgs_2_2 x_reqs_2
      x_error = ((:) x_error_1 x_error_2)
      x_fmts = ((:) x_fmts_1 x_fmts_2)
lambda_NilArgs_2 x_reqs = (x_error , x_fmts)
  where
      x_fmts = ([] )
      x_error = ([] )
lambda_ConsFillList_1 t_PPS_1 t_FillList_2_1 x_minlli x_minwi x_maxhi = (lambda_ConsFillList_2 x_newll x_maxhi x_maxh_1 t_PPS_2 t_FillList_2_2 , x_maxh_2 , x_minll_2 , x_minw_2)
  where
      (t_PPS_2,x_maxh_1 , x_minll_1 , x_minw_1) = t_PPS_1
      x_maxhi_2 = (cons_height x_maxh_1 x_maxhi True)
      x_newll = (x_minlli+x_minw_1)
      x_minlli_2 = ((x_minlli+x_minwi)+x_minw_1)
      (t_FillList_2_2,x_maxh_2 , x_minll_2 , x_minw_2) = t_FillList_2_1 x_minlli_2 x_minwi x_maxhi_2
lambda_NilFillList_1 x_minlli x_minwi x_maxhi = (lambda_NilFillList_2 , x_maxhi , x_minlli , x_minwi)
lambda_ConsFillList_2 x_newll x_maxhi x_maxh_1 t_PPS_2 t_FillList_2_2 x_errori x_fmtsi x_frame x_pw = (x_error , x_fmts_2)
  where
      (x_error_1 , x_fmts_1) = t_PPS_2 x_frame
      x_avail = ((x_pw-x_newll)>=0)
      x_fe = (set_fmts_filllist x_fmtsi x_fmts_1 x_maxhi x_maxh_1 x_frame x_avail)
      x_ferror = (snd_Pair_Formats x_fe)
      x_errori_2 = (x_errori||x_ferror)
      x_fmtsi_2 = (fst_Pair_Formats x_fe)
      (x_error_2 , x_fmts_2) = t_FillList_2_2 x_errori_2 x_fmtsi_2 x_frame x_pw
      x_error = (x_error_2||x_error_1)
lambda_NilFillList_2 x_errori x_fmtsi x_frame x_pw = (x_errori , x_fmtsi)
afmt_txt string =
 (C_AFormat_1 (text_fmts string))
asts i =
 (if (i==0) then "" else (if (i==1) then "*" else ((":"++(repeatCHAR '*' (toInt (i-2))))++"*>")) )
beside_height lh rh =
 (if ((lh==0)||(rh==0)) then 0 else 1)
choose_ab_beside_fmts avail fa fb f =
 (if avail then (beside_fmts f fa fb) else (above_fmts fa fb))
choose_ab_error_beside avail fa fb f =
 (if avail then (error_beside fa fb) else (above_fmts fa fb))
cons_height pph acth avail =
 (if (acth==0) then (if (pph>0) then 1 else 0) else (acth+(if avail then 0 else 1) ))
eq_all_fmts pw fmts =
 (allf (set_fmts_render pw fmts))
eq_best_fmts pw fmts =
 (best (set_fmts_render pw fmts))
-- JAS 2021!
eq_disp_fmts pw fmts =
 (dispf (set_fmts_render pw fmts))
 
eq_set_error_msg apars bpars =
 (((("<Error: incorrect choice expression. #pars left "++(lrc_INTtoSTR apars))++" /= #pars right ")++(lrc_INTtoSTR bpars))++">")
eq_set_fmts_apply error msg fmts =
 (set_fmts_apply error (C_AFormat_1 (text_fmts msg)) fmts)
eq_set_fmts_fill fmts =
 (C_AFormat_1 fmts)
fst_Pair_Formats f =
 case f of { (C_C_Pair_Formats_1 a b) -> a }
fst_Pair_T_Formats f =
 case f of { (C_C_Pair_T_Formats_1 a b) -> a }
fst_t t =
 case t of { (C_Triple_1 pw _ _) -> pw }
get_fmts fs =
 case fs of { (C_AFormat_1 a) -> a ; (C_TFormats_1 _ _ _ _) -> (text_fmts "<Error: can\'t dup a dup") }
narrow_frame i f =
 case f of { (C_F_1 s l) -> (C_F_1 (s-i) (l-i)) }
narrow_ll i f =
 case f of { (C_F_1 s l) -> (C_F_1 s (l-i)) }
nullFormats fs =
 case fs of { ( [] ) -> True ; _ -> False }
sem_fmts_cdup a b ae be anpars bnpars min_w mesg =
 (if (anpars/=bnpars) then (afmt_txt mesg) else (sem_fmts_dup a b ae be min_w))
sem_fmts_dup afs bfs ae be minw =
 (if (ae && be) then (afmt_txt (asts minw)) else (C_TFormats_1 (get_fmts afs) (get_fmts bfs) ae be))
set_error_msg numpars len =
 (((("<Error: incorrect apply expression. #pars "++(lrc_INTtoSTR numpars))++" /= #args ")++(lrc_INTtoSTR len))++">")
set_fmts_ab_above fs gs uh lh etxt =
 case fs of { (C_AFormat_1 ffmts) -> case gs of { (C_AFormat_1 gfmts) -> (C_C_Pair_T_Formats_1 (C_AFormat_1 (set_fmts_abovea ffmts gfmts uh lh)) False) ; (C_TFormats_1 a bs ae be) -> (C_C_Pair_T_Formats_1 (C_TFormats_1 (set_fmts_abovea ffmts a uh lh) (set_fmts_abovea ffmts bs uh lh) ae be) False) } ; (C_TFormats_1 a bs ae be) -> case gs of { (C_AFormat_1 gfmts) -> (C_C_Pair_T_Formats_1 (C_TFormats_1 (set_fmts_abovea a gfmts uh lh) (set_fmts_abovea bs gfmts uh lh) ae be) False) ; _ -> (C_C_Pair_T_Formats_1 (afmt_txt etxt) True) } }
set_fmts_ab_beside fs gs uh lh frame error etxt =
 case fs of { (C_AFormat_1 ffmts) -> case gs of { (C_AFormat_1 gfmts) -> (C_C_Pair_T_Formats_1 (C_AFormat_1 (set_fmts_besidea ffmts gfmts uh lh frame error)) False) ; (C_TFormats_1 a bs ae be) -> (C_C_Pair_T_Formats_1 (C_TFormats_1 (set_fmts_besidea ffmts a uh lh frame error) (set_fmts_besidea ffmts bs uh lh frame error) ae be) False) } ; (C_TFormats_1 a bs ae be) -> case gs of { (C_AFormat_1 gfmts) -> (C_C_Pair_T_Formats_1 (C_TFormats_1 (set_fmts_besidea a gfmts uh lh frame error) (set_fmts_besidea bs gfmts uh lh frame error) ae be) False) ; _ -> (C_C_Pair_T_Formats_1 (afmt_txt etxt) True) } }
set_fmts_above us ls uh lh =
 (set_fmts_ab_above us ls uh lh "<Error: can\'t above two pairs")
set_fmts_abovea a bs uh lh =
 (if (uh==0) then bs else (if (lh==0) then a else (above_fmts a bs)) )
set_fmts_apply error msg fmts =
 (if error then msg else fmts)
set_fmts_beside ls rs lh rh frame error =
 (set_fmts_ab_beside ls rs lh rh frame error "<Error: can\'t beside two pairs")
set_fmts_besidea ls rs lh rh frame error =
 (if (lh==0) then rs else (if (rh==0) then ls else (if error then (error_beside ls rs) else (beside_fmts frame ls rs)) ) )
set_fmts_empty =
 (C_AFormat_1 (empty_fmts ))
set_fmts_fillblock i fmts w =
 (if (i<0) then (afmt_txt "<Error: negative page width in fillblock>") else (if (i>w) then (afmt_txt (asts i)) else (C_AFormat_1 fmts)) )
set_fmts_filllist af bf ah bh f avail =
 case bf of { (C_AFormat_1 ns) -> (if (ah==0) then (C_C_Pair_Formats_1 ns False) else (if (bh==0) then (C_C_Pair_Formats_1 af False) else (if (bh<=1) then (C_C_Pair_Formats_1 (choose_ab_beside_fmts avail af ns f) False) else (C_C_Pair_Formats_1 (choose_ab_error_beside avail af (text_fmts "<Error: element in fill higher than 1>") f) True)) ) ) ; _ -> (C_C_Pair_Formats_1 (set_fmts_filllista (text_fmts "<Error: element in fill list is a pair>") af ah bh avail f) True) }
set_fmts_filllista fs afmts ah nh avail f =
 (if (ah==0) then afmts else (if (nh==0) then fs else (choose_ab_error_beside avail fs afmts f)) )
set_fmts_indent i f pw minw frame error =
 (if (i<0) then (afmt_txt "<Error: negative indentation>") else (if (i>pw) then (afmt_txt (asts minw)) else (if error then (set_fmts_indent_error_indent i f) else (set_fmts_indent_indent_fmts i f frame)) ) )
set_fmts_indent_error_indent i fmts =
 case fmts of { (C_AFormat_1 fs) -> (C_AFormat_1 (error_indent i fs)) ; (C_TFormats_1 a bs ae be) -> (C_TFormats_1 (error_indent i a) (error_indent i bs) ae be) }
set_fmts_indent_indent_fmts i fmts frame =
 case fmts of { (C_AFormat_1 fs) -> (C_AFormat_1 (indent_fmts frame i fs)) ; (C_TFormats_1 a bs ae be) -> (C_TFormats_1 (indent_fmts frame i a) (indent_fmts frame i bs) ae be) }
set_fmts_join fs err =
 case fs of { (C_AFormat_1 f) -> (if err then (C_C_Pair_T_Formats_1 fs err) else (C_C_Pair_T_Formats_1 (afmt_txt "<Error: can\'t join a single result>") True)) ; (C_TFormats_1 a bs ae be) -> let { (aa) = (if be then (if (nullFormats a) then bs else a) else (if ae then (if (nullFormats bs) then a else bs) else (merge a bs)) ) } in (C_C_Pair_T_Formats_1 (C_AFormat_1 aa) False) }
set_fmts_render pw fmts =
 (if (pw<0) then (text_fmts "<Error: negative page width >") else case fmts of { (C_AFormat_1 fmt) -> fmt ; _ -> (text_fmts "<Error: can\'t render a pair>") } )
set_fmts_text string minw error =
 (afmt_txt (if error then (asts minw) else string) )
snd_Pair_Formats f =
 case f of { (C_C_Pair_Formats_1 a b) -> b }
snd_Pair_T_Formats f =
 case f of { (C_C_Pair_T_Formats_1 a b) -> b }
snd_t t =
 case t of { (C_Triple_1 _ pll _) -> pll }
third_t t =
 case t of { (C_Triple_1 _ _ ph) -> ph }
set_var_apply cond va vb = if cond then va else vb


eq_DupC opta_reqs optb_reqs = zipWith max opta_reqs optb_reqs