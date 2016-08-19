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

## Helper function
intubate <-
  
  ## Deprecated helper functions
  ## (for compatibility with 0.99.2. Will be removed at some point)
  ntbt_function_formula_data <-
  ntbt_function_model_data <-
  ntbt_function_object_data <-
  ntbt_function_x_data <-
  
  ## Interface functions
  
  ## adabag
  ntbt_autoprune <-
  ntbt_bagging <-
  ntbt_bagging.cv <-
  ntbt_boosting <-
  ntbt_boosting.cv <-
  
  ## AER
  ntbt_ivreg <-
  ntbt_tobit <-
  
  ## aod
  ntbt_betabin <-
  ntbt_donner <-
  ntbt_negbin <-
  ntbt_quasibin <-
  ntbt_quasipois <-
  
  ntbt_raoscott <-
  ntbt_splitbin <-

  ## ape
  ntbt_binaryPGLMM <-
  ntbt_compar.gee <-
  ntbt_correlogram.formula <-
  ntbt_lmorigin <-
  ntbt_yule.cov <-
  
  ## arm
  ntbt_bayesglm <-
  ntbt_bayespolr <-
  
  ## betareg
  ntbt_betamix <-
  ntbt_betareg <-
  ntbt_betatree <-

  ## brglm
  ntbt_brglm <-
  
  ## caper
  ntbt_brunch <-
  ntbt_crunch <-
  ntbt_macrocaic <-
  ntbt_pgls <-

  ## car
  ntbt_Boxplot <-
  ntbt_boxTidwell <-
  ntbt_densityPlot <-
  ntbt_invTranPlot <-
  ntbt_leveneTest <-
  
  ntbt_powerTransform <-
  ntbt_scatter3d <-
  ntbt_scatterplot <-
  ntbt_scatterplotMatrix <-
  ntbt_spreadLevelPlot <-
  
  ntbt_symbox <-
  
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

  ## coin
  ntbt_ansari_test <-
  ntbt_chisq_test <-
  ntbt_cmh_test <-
  ntbt_conover_test <-
  ntbt_fisyat_test <-
  
  ntbt_fligner_test <-
  ntbt_friedman_test <-
  ntbt_independence_test <-
  ntbt_klotz_test <-
  ntbt_koziol_test <-
  
  ntbt_kruskal_test <-
  ntbt_lbl_test <-
  ntbt_logrank_test <-
  ntbt_maxstat_test <-
  ntbt_median_test <-
  
  ntbt_mh_test <-
  ntbt_mood_test <-
  ntbt_normal_test <-
  ntbt_oneway_test <-
  ntbt_quade_test <-
  
  ntbt_quadrant_test <-
  ntbt_sign_test <-
  ntbt_symmetry_test <-
  ntbt_taha_test <-
  ntbt_savage_test <-
  
  ntbt_spearman_test <-
  ntbt_wilcox_test <-
  ntbt_wilcoxsign_test <-
  
  ## CORElearn
  ntbt_attrEval <-
  ntbt_CoreModel <-
  ntbt_discretize <-
  ntbt_ordEval <-
  
  ## drc
  ntbt_drm <-

  ## e1071
  ntbt_svm <-
  
  ## earth
  ntbt_earth <-

  ## EnvStats
  ## ntbt_calibrate <-    ## It needs an attach() in interface. Disabled for now.
  ntbt_gofTest <-
  ntbt_gofGroupTest <-
  ntbt_kendallSeasonalTrendTest <-
  ntbt_kendallTrendTest <-
  
  ntbt_stripChart <-
  ntbt_summaryFull <-
  ntbt_summaryStats <-
  ntbt_varGroupTest <-

  ## fGarch
  ntbt_garchFit <-

  ## forecast
  ntbt_tslm <-
  
  ## gam
  ntbt_gam <-
  
  ## gbm
  ntbt_gbm <-
  
  ## gee
  ntbt_gee <-

  ## glmnet
  ntbt_glmnet <-
  ntbt_cv.glmnet <-
  
  ## glmx
  ntbt_glmx <-
  ntbt_hetglm <-
  
  ## gmnl
  ntbt_gmnl <-
  
  ## gplots  
  ntbt_bandplot <-
  ntbt_lowess <-
  ntbt_overplot <-
  ntbt_plotmeans <-

  ## gss  
  ntbt_gssanova <-
  ntbt_gssanova0 <-
  ntbt_gssanova1 <-
  ntbt_ssanova <-
  ntbt_ssanova0 <-
  
  ntbt_ssanova9 <-
  ntbt_sscden <-
  ntbt_sscden1 <-
  ntbt_sscox <-
  ntbt_ssden <-
  
  ntbt_ssden1 <-
  ntbt_sshzd <-
  ntbt_ssllrm <-
  
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
  
  ## ipred
  ntbt_bagging <-
  ntbt_errorest <-
  ntbt_inbagg <-
  ntbt_inclass <-
  ntbt_slda <-
  
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
  
  ## kknn
  ntbt_train.kknn <-
  ntbt_cv.kknn <-
  ntbt_kknn <-
  
  ## klaR
  ntbt_classscatter <-
  ntbt_cond.index <-
  ntbt_greedy.wilks <-
  ntbt_loclda <-
  ntbt_meclight <-
  
  ntbt_NaiveBayes <-
  ntbt_nm <-
  ntbt_partimat <-
  ntbt_plineplot <-
  ntbt_pvs <-
  
  ntbt_rda <-
  ntbt_sknn <-
  ntbt_stepclass <-
  ntbt_woe <-
  
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
  
  ## lars
  ntbt_lars <-
  ntbt_cv.lars <-
  
  ## latticeExtra
  ntbt_ecdfplot <-
  ntbt_mapplot <-
  ntbt_rootogram <-
  ntbt_segplot <-
  ntbt_tileplot <-

  ## leaps
  ntbt_regsubsets <-
  
  ## lfe
  ntbt_felm <-
  
  ## lme4
  ntbt_glmer <-
  ntbt_glmer.nb <-
  ntbt_glFormula <-
  ntbt_lFormula <-
  ntbt_lmer <-
  
  ntbt_lmList <-
  ntbt_nlmer <-

  ## lmtest
  ntbt_bgtest <-
  ntbt_bptest <-
  ntbt_coxtest <-
  ntbt_dwtest <-
  ntbt_encomptest <-
  
  ntbt_gqtest <-
  ntbt_grangertest <-
  ntbt_harvtest <-
  ntbt_hmctest <-
  ntbt_jtest <-
  
  ntbt_raintest <-
  ntbt_resettest <-

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
  
  ## MCMCglmm
  ntbt_MCMCglmm <-

  ## mda
  ntbt_fda <-
  ntbt_mda <-
  
  ## metafor
  ntbt_escalc <-
  ntbt_rma <-

  ## mgcv
  ntbt_bam <-
  ntbt_gam <-
  ntbt_gamm <-
  
  ## mhurdle
  ntbt_mhurdle <-
  
  ## minpack.lm
  ntbt_nlsLM <-
  
  ## mlogit
  ntbt_mlogit <-
  
  ## modeltools
  ntbt_ModelEnvFormula <-
  ntbt_ParseFormula <- 

  ## nlme
  ntbt_gls <-
  ntbt_lme <-
  ntbt_lmList <-
  ntbt_nlme <-
  ntbt_nlsList <-
  
  ## nlreg
  ntbt_nlreg <-

  ## nnet
  ntbt_multinom <-
  ntbt_nnet <-
  
  ## ordinal
  ntbt_clm <-
  ntbt_clm2 <-
  ntbt_clmm <-
  
  ## party
  ntbt_cforest <-
  ntbt_ctree <-
  ntbt_mob <-

  ## plotrix
  ntbt_barNest <-
  ntbt_brkdn.plot <-
  ntbt_brkdnNest <-
  ntbt_histStack <-
  ntbt_plotH <-
  
  ## pls
  ntbt_cppls <-
  ntbt_mvr <-
  ntbt_pcr <-
  ntbt_plsr <-
  
  ## pROC
  ntbt_auc <-
  ntbt_ci <-
  ntbt_ci.auc <-
  ntbt_ci.coords <-
  ntbt_ci.se <-
  
  ntbt_ci.sp <-
  ntbt_ci.thresholds <-
  ntbt_multiclass.roc <-
  ntbt_plot.roc <-
  ntbt_roc <-

  ## pscl
  ntbt_hurdle <-
  ntbt_zeroinfl <-

  ## quantreg
  ntbt_dynrq <-
  ntbt_KhmaladzeTest <-
  ntbt_nlrq <-
  ntbt_rq <-
  ntbt_rqProcess <-
  
  ntbt_rqss <-
  
  ## randomForest
  ntbt_randomForest <-
  
  ## Rchoice
  ntbt_Rchoice <-

  ## rminer
  ntbt_fit <-
  ntbt_mining <-
  
  ## rms  
  ntbt_bj <-
  ntbt_cph <-
  ntbt_Glm <-
  ntbt_lrm <-
  ntbt_npsurv <-
  
  ntbt_ols <-
  ntbt_orm <-
  ntbt_psm <-
  
  ## robustbase
  ntbt_adjbox <-
  ntbt_glmrob <-
  ntbt_lmrob <-
  ntbt_ltsReg <-
  ntbt_nlrob <-

  ## rpart
  ntbt_rpart <-
  
  ## RRF
  ntbt_RRF <-
  ntbt_rrfImpute <-

  #RWeka
  ntbt_AdaBoostM1 <-
  ntbt_Bagging <-
  ntbt_CostSensitiveClassifier <-
  ntbt_DecisionStump <-
  ntbt_Discretize <-
  
  ntbt_GainRatioAttributeEval <-
  ntbt_IBk <-
  ntbt_InfoGainAttributeEval <-
  ntbt_J48 <-
  ntbt_JRip <-
  
  ntbt_LBR <-
  ntbt_LogitBoost <-
  ntbt_LinearRegression <-
  ntbt_LMT <-
  ntbt_Logistic <-
  
  ntbt_M5P <-
  ntbt_M5Rules <-
  ntbt_MultiBoostAB <-
  ntbt_Normalize <-
  ntbt_OneR <-
  
  ntbt_PART <-
  ntbt_SMO <-
  ntbt_Stacking <- 
  
  ## sem
  ntbt_rawMoments <-
  ntbt_sem <-
  ntbt_tsls <-
  
  ## spBayes
  ntbt_bayesGeostatExact <-
  ntbt_bayesLMConjugate <-
  ntbt_spDynLM <-

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
  ntbt_survreg <-
  ntbt_survSplit <-
  
  ## SwarmSVM
  ntbt_alphasvm <- 

  ## tree
  ntbt_tree <-
  
  ## vcd
  ntbt_agreementplot <-
  ntbt_assoc <-
  ntbt_cd_plot <-
  ntbt_cotabplot <-
  ntbt_loddsratio <-
  
  ntbt_mosaic <-
  ntbt_sieve <-
  ntbt_spine <-
  ntbt_structable <-

  ## vegan
  ntbt_adipart <-
  ntbt_adonis <-
  ntbt_adonis2 <-
  ntbt_bioenv <-
  ntbt_capscale <-
  
  ntbt_cca <-
  ntbt_dbrda <-
  ntbt_envfit <-
  ntbt_gdispweight <-
  ntbt_multipart <-
  
  ntbt_ordicloud <-
  ntbt_ordisplom <-
  ntbt_ordisurf <-
  ntbt_ordixyplot <-

  ## (external)
  ## intubate function
  function(data, ...) {
    preCall <- match.call(expand.dots = FALSE)
    
    Call <- match.call(expand.dots = TRUE)
    
    cfti <- get_calling_name("ntbt", as.character(Call[[1]]))
    
    result <- process_call(data, preCall, Call, cfti, parent.frame())
    
    if (result$result_visible)
      return (result$result)
    invisible(result$result)
  }
