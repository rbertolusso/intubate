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

function_data <- function(data, ...) data

ntbt_function_data <-
  
  ## graphics
  ntbt_boxplot <-
  ntbt_cdplot <-
  ntbt_coplot <-
  ntbt_mosaicplot <-
  ntbt_pairs <-
  ntbt_plot <-
  ntbt_spineplot <-
  ntbt_sunflowerplot <-
  ntbt_text <-
  
  ## nnet
  ntbt_multinom <-
  ntbt_nnet <-
  
  
  ## randomForest
  ntbt_randomForest <-
  
  


  
  
    
  ## e1071
  ntbt_svm <-
  
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

  ## pls
  ntbt_cppls <-
  ntbt_mvr <-
  ntbt_pcr <-
  ntbt_plsr <-
  
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
  
  
  ## survival
  ntbt_cch <-
  ntbt_coxph <-
  ntbt_finegray <-
  ntbt_pyears <-
  ntbt_survConcordance <-
  ntbt_survexp <-
  ntbt_survfit <-
  ntbt_survSplit <-
  
  ## tree
  ntbt_tree <-
  
  ## graphics
  ntbt_stripchart <-
  
  ## lattice
  ntbt_barchart <-
  ntbt_cloud <-
  ntbt_bwplot <-
  ntbt_contourplot <-
  ntbt_dotplot <-
  ntbt_densityplot <-
  ntbt_histogram <-
  ntbt_levelplot <-
  ntbt_oneway <-
  ntbt_parallelplot <-
  ntbt_qq <-
  ntbt_qqmath <-
  ntbt_splom <-
  ntbt_stripplot <-
  ntbt_wireframe <-
  ntbt_xyplot <-

  
  ## leaps
  ntbt_regsubsets <-
  ntbt_tmd <-
  
  
  ## nlme
  ntbt_gls <-
  ntbt_lme <-
  ntbt_lmList <-
  ntbt_nlme <-
  ntbt_nlsList <-

  ## gam
  ntbt_gam <-
  
  ## gbm
  ntbt_gbm <-

  ## The function below seems to address, for now,
  ## *all* the cases that 0.99.2 was able to
  ## (functions with a formula, with whichever
  ## name variant the formula has).
  ## Moreover, now it seems it can address
  ## non-formula variants too. Needs testing.
  function(data, ...) {
    Call <- match.call(expand.dots = FALSE)

    ## If there is a formula, it is assumed following data.
    formula <- as.character(Call$...[[1]])
    ## Trying to determine if it is a formula.
    is_formula <- (formula[1] == "~" && length(formula) >= 2 && length(formula) <= 3)
    ## Better way?

    Call <- match.call(expand.dots = TRUE)
    Call[[1]] <- get_calling_name("ntbt", as.character(Call[[1]]))

    if (is_formula) {
      Call[[2]] <- as.name("data")
      Call[2:3] <- Call[3:2]             ## Switch parameters
      names(Call)[2:3] <- c("", "data")  ## Leave formula unnamed
      ret <- eval(Call)
    } else  {
      ret <- with(data, eval(Call[-2]))  ## Need to remove data [-2]
    }
    if (!is.null(ret))
      return(ret)
    invisible(data) ## If no value returned, forward data invisibly
  }


get_calling_name <- function(prefix, full_name) {
  ## There are two possibilities:
  ## 1) <prefix>_<name>           (1 element  => <prefix>_<name>)
  ## 2) intubate::<prefix>_<name> (3 elements => ::, intubate, <prefix>_<name> )
  full_name <- full_name[length(full_name)]
  ## Check the prefix is as expected, and at least one character
  ## after the _ (we do not check if it starts with . or letter,
  ## that error can be caught by other functions).
  ## The important thing is to avoid C stack errors.
  if (gsub(paste0("(", prefix, ")_.+"), "\\1", full_name) != prefix)
    stop(paste0(full_name, 
                " is an invalid name.\n", 
                "The interface should be named ", prefix, "_<name>\n",
                "where <name> is the name of the function to be interfaced."))
  as.name(gsub(paste0(prefix, "_(.+)"), "\\1", full_name))
}
