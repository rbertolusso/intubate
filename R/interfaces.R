## Copyright (C) 2016 Roberto Bertolusso
##
## This file is part of intubate.
##
## intubate is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 2 of the License, or
## (at your option) any later version.
##
## intubate is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with intubate. If not, see <http://www.gnu.org/licenses/>.

## External functions
intubate <-
  
  ## Interface functions
  
  ## caret
  ntbt_avNNet <-
  ntbt_bagEarth <-
  ntbt_bagFDA <-
  ntbt_calibration <-
  ntbt_dummyVars <-
  
  ntbt_icr <-
  ntbt_knn3 <-
  ntbt_lift <-
  ntbt_pcaNNet <-
  ntbt_sbf <-
  
  ntbt_train <-

  ## e1071
  ntbt_svm <-
  
  ## gam
  ntbt_gam <-
  
  ## gbm
  ntbt_gbm <-
  
  ## graphics
  ntbt_boxplot <-
  ntbt_cdplot <-
  ntbt_coplot <-
  ntbt_mosaicplot <-
  ntbt_pairs <-
  
  ntbt_plot <-
  ntbt_spineplot <-
  ntbt_sunflowerplot <-
  ntbt_stripchart <-
  ntbt_text <-
  
  ## Hmisc
  ntbt_areg.boot <-
  ntbt_aregImpute <-
  ntbt_biVar <-
  ntbt_bpplotM <-
  ntbt_dataRep <-
  
  ntbt_describe <-
  ntbt_Dotplot <-
  ntbt_Ecdf <-
  ntbt_fit.mult.impute <-
  ntbt_nobsY <-
  
  ntbt_rcorrcens <-
  ntbt_redun <-
  ntbt_summary <-
  ntbt_summaryD <-
  ntbt_summaryM <-
  
  ntbt_summaryP <-
  ntbt_summaryRc <-
  ntbt_summaryS <-
  ntbt_transcan <-
  ntbt_varclus <-
  
  ntbt_xYplot <-
  
  ## iRegression
  ntbt_bivar <-
  ntbt_ccrm <-
  ntbt_cm <-
  ntbt_crm <-
  ntbt_MinMax <-
  
  ## kernlab
  ntbt_gausspr <-
  ntbt_kfa <-
  ntbt_kha <-
  ntbt_kkmeans <-
  ntbt_kpca <-
  
  ntbt_kqr <-
  ntbt_ksvm <-
  ntbt_lssvm <-
  ntbt_rvm <-
  ntbt_sigest <-
  
  ntbt_specc <-
  
  ## lattice
  ntbt_barchart <-
  ntbt_bwplot <-
  ntbt_cloud <-
  ntbt_contourplot <-
  ntbt_densityplot <-
  
  ntbt_dotplot <-
  ntbt_histogram <-
  ntbt_levelplot <-
  ntbt_oneway <-
  ntbt_parallelplot <-
  
  ntbt_qq <-
  ntbt_qqmath <-
  ntbt_splom <-
  ntbt_stripplot <-
  ntbt_tmd <-
  
  ntbt_wireframe <-
  ntbt_xyplot <-
  
  ## leaps
  ntbt_regsubsets <-
  
  ## lfe
  ntbt_felm <-
  
  ## MASS
  ntbt_corresp <-
  ntbt_glm.nb <-
  ntbt_lda <-
  ntbt_lm.gls <-
  ntbt_lm.ridge <-
  
  ntbt_loglm <-
  ntbt_logtrans <-
  ntbt_polr <-
  ntbt_qda <-
  ntbt_rlm <-
  
  ## modeltools
  ntbt_ModelEnvFormula <-
  ntbt_ParseFormula <- 

  ## nlme
  ntbt_gls <-
  ntbt_lme <-
  ntbt_lmList <-
  ntbt_nlme <-
  ntbt_nlsList <-
  
  ## nnet
  ntbt_multinom <-
  ntbt_nnet <-
  
  ## party
  ntbt_cforest <-
  ntbt_ctree <-
  ntbt_mob <-

  ## pls
  ntbt_cppls <-
  ntbt_mvr <-
  ntbt_pcr <-
  ntbt_plsr <-
  
  ## randomForest
  ntbt_randomForest <-

  ## rms  
  ntbt_bj <-
  ntbt_cph <-
  ntbt_Glm <-
  ntbt_lrm <-
  ntbt_npsurv <-
  
  ntbt_ols <-
  ntbt_orm <-
  ntbt_psm <-

  ## rpart
  ntbt_rpart <-
  
  ## stats
  ntbt_aggregate <-
  ntbt_alias <-
  ntbt_ansari.test <-
  ntbt_aov <-
  ntbt_bartlett.test <-
  
  ntbt_cor.test <-
  ntbt_fligner.test <-
  ntbt_friedman.test <-
  ntbt_ftable <-
  ntbt_getInitial <-
  
  ntbt_glm <-
  ntbt_kruskal.test <-
  ntbt_lm <-
  ntbt_loess <-
  ntbt_lqs <-
  
  ntbt_model.frame <-
  ntbt_model.matrix <-
  ntbt_mood.test <-
  ntbt_nls <-
  ntbt_oneway.test <-
  
  ntbt_ppr <-
  ntbt_prcomp <-
  ntbt_princomp <-
  ntbt_quade.test <-
  ntbt_replications <-
  
  ntbt_t.test <-
  ntbt_var.test <-
  ntbt_wilcox.test <-
  ntbt_xtabs <-
  
  ## strucchange
  ntbt_breakpoints <-
  ntbt_efp <-
  ntbt_Fstats <-
  ntbt_mefp <-
  ntbt_recresid <-
  
  ntbt_sctest <-
  
  ## survey
  ntbt_svyby <-
  ntbt_svycoxph <-
  ntbt_svydesign <-
  ntbt_svyglm <-
  ntbt_svymean <-
  
  ntbt_svyquantile <-
  ntbt_svyratio <-
  ntbt_svytotal <-
  ntbt_twophase <-

  ## survival
  ntbt_cch <-
  ntbt_coxph <-
  ntbt_pyears <-
  ntbt_survConcordance <-
  ntbt_survexp <-
  
  ntbt_survfit <-
  ntbt_survSplit <-
  
  ## tree
  ntbt_tree <-
  
  ## (external)
  ## intubate function
  function(data, ...) {
    preCall <- match.call(expand.dots = FALSE)
    
    Call <- match.call(expand.dots = TRUE)
    Call[[1]] <- get_calling_name("ntbt", as.character(Call[[1]]))
    
    result <- process_call(data, preCall, Call, parent.frame())
    
    if (result$result_visible)
      return (result$result)
    invisible(result$result)
  }
