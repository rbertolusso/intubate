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

## Interface function
intubate <-
  
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
  
  ## nlme
  ntbt_gls <-
  ntbt_lme <-
  ntbt_lmList <-
  ntbt_nlme <-
  ntbt_nlsList <-
  
  ## nnet
  ntbt_multinom <-
  ntbt_nnet <-
  
  ## pls
  ntbt_cppls <-
  ntbt_mvr <-
  ntbt_pcr <-
  ntbt_plsr <-
  
  ## randomForest
  ntbt_randomForest <-
  
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
  
  ## intubate function
  function(data, ...) {
    preCall <- match.call(expand.dots = FALSE)
    
    Call <- match.call(expand.dots = TRUE)
    Call[[1]] <- get_calling_name("ntbt", as.character(Call[[1]]))

    if (length(preCall$...) == 0)  {
      ## cat("No arguments other than data\n")
      ## print(Call)
      result <- eval(Call)
    } else if (there_are_formulas(preCall$...)) {
      ## cat("Formula\n")
      result <- process_formula_case(Call, use_envir = parent.frame())      
    } else  {
      ## cat("Rest of cases\n")
      ## print(Call)
      result <- with(data, eval(Call[-2]))    ## Remove "data" [-2] before calling
    }
    if (!is.null(result)) {
      if (withVisible(result)$visible)
        return (result)
      else
        data <- result
    }
    invisible(data)
  }


## (internal)
## This special case for formulas is, at least for now, needed because
## "Rest of cases" below does not know how to manage cases with "." in
## a formula (and the called function neither because only sees the variables
## inside the data, not the data itself). An alternative could be to have in
## "Rest of cases" some sort of "stats::model.matrix" call (of which I have
## not clue how to implement), for *all* the formulas that have a ".", so maybe
## the end result will be even more involved and this is as good as we can do.
process_formula_case <- function(Call, use_envir) {
  Call[2:3] <- Call[3:2]                       ## Switch parameters
  names(Call)[2:3] <- c("", "data")            ## Leave formula unnamed
  ## print(Call)
  result <- try(eval(Call, envir = use_envir), silent = TRUE)
  if (class(result)[[1]] == "try-error") {     ## Maybe data has other name
    names(Call)[[3]] <- ""                     ## Leave data unnamed
    ## print(Call)
    result <- try(eval(Call, envir = use_envir), silent = TRUE)   ## Retry
    if (class(result)[[1]] == "try-error") {   ## Maybe data is in position 3
      Call[3:4] <- Call[4:3]                   ## Switch parameters
      ## print(Call)
      result <- eval(Call, envir = use_envir)  ## Retry. If error, give up
    }
  }
  result
}

## This special case for formulas is, at least for now, needed because
## "Rest of cases" below does not know how to manage cases with "." in
## a formula (and the called function neither because only sees the variables
## inside the data, not the data itself). An alternative could be to have in
## "Rest of cases" some sort of "stats::model.matrix" call (of which I have
## not clue how to implement), for *all* the formulas that have a ".", so maybe
## the end result will be even more involved and this is as good as we can do.
process_formula_case_bak1 <- function(Call, use_envir) {
  signal_error <- TRUE
  ## Let's have "data" take a walk until it finds its place in the world,
  ## as functions are supposed to check if unnamed parameters are sent
  ## in the wrong order (Right?).
  for (par in 3:length(names(Call))) {
    Call[(par-1):par] <- Call[par:(par-1)]        ## Switch parameters
    names(Call)[(par-1):par] <- names(Call)[par:(par-1)]  ## and names
    print(Call)
    result <- try(eval(Call, envir = use_envir),  ## See if it flies
                  silent = TRUE)
    if (class(result)[[1]] != "try-error") {          ## Did. We are done
      signal_error <- FALSE
      break
    }
  }
  if (signal_error) {          ## Parameters exhausted and still error
    print(Call)                ## Show call of last attempt
    stop(result)               ## Give up
  }
  result
}

## This special case for formulas is, at least for now, needed because
## "Rest of cases" below does not know how to manage cases with "." in
## a formula (and the called function neither because only sees the variables
## inside the data, not the data itself). An alternative could be to have in
## "Rest of cases" some sort of "stats::model.matrix" call (of which I have
## not clue how to implement), for *all* the formulas that have a ".", so maybe
## the end result will be even more involved and this is as good as we can do.
process_formula_case_bak2 <- function(Call) {
  signal_error <- TRUE
  ## Let's have "data" take a walk until it finds its place in the world,
  ## as functions are supposed to check if unnamed parameters are sent
  ## in the wrong order (Right?).
  for (par in 3:length(names(Call))) {
    Call[(par-1):par] <- Call[par:(par-1)]        ## Switch parameters
    names(Call)[(par-1):par] <- names(Call)[par:(par-1)]  ## and names
    ## print(Call)
    result <- try(eval(Call, envir = parent.frame()),  ## See if it flies
                  silent = TRUE)
    if (class(result)[[1]] != "try-error") {          ## Did. We are done
      signal_error <- FALSE
      break
    }
  }
  if (signal_error) {          ## Parameters exhausted and still error
    for (data_name in c("design")) {  ## Start assigning other names to data
      names(Call)[[par]] <- data_name
      result <- try(eval(Call, envir = parent.frame()),  ## See if it flies
                    silent = TRUE)
      if (class(result)[[1]] != "try-error") {          ## Did. We are done
        signal_error <- FALSE
        break
      }
    }
    print(Call)                ## Show call of last attempt
    stop(result)               ## Give up
  }
  if (signal_error) {          ## Parameters exhausted and still error
    print(Call)                ## Show call of last attempt
    stop(result)                  ## Give up
  }
  result
}


## Determine if there is a formula (internal)
there_are_formulas <- function(par_list) {
  sum(sapply(par_list,
             function(par) {
               is_formula <- try(inherits(as.formula(as.character(par)),
                                          "formula"), silent = TRUE)
               !(class(is_formula)[[1]] == "try-error")
             }) > 0)
}

## Name checking (internal)
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
