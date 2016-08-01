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

get_calling_name <- function(prefix, full_name) {
  ## There are two possibilities:
  ## 1) <prefix>_<name>           (1 element  => <prefix>_<name>)
  ## 2) intubate::<prefix>_<name> (3 elements => ::, intubate, <prefix>_<name> )
  full_name <- full_name[length(full_name)]
  ## Check the prefix is as expected, and at least one character
  ## after the _ (we do not check if it starts with . or letter).
  ## The important thing is to avoid C stack error.
  if (gsub(paste0("(", prefix, ")_.+"), "\\1", full_name) != prefix)
    stop(paste0(full_name, 
                " is an invalid name.\n", 
                "The interface should be named ", prefix, "_<name>\n",
                "where <name> is the name of the function to be interfaced."))
  as.name(gsub(paste0(prefix, "_(.+)"), "\\1", full_name))
}

function_. <- function(.) .

## Functions with any number of parameters. It is suitable for
## functions such as print() or View(), that return NULL.
prmt_function_. <-
  prmt_print <- 
  prmt_View <-
  
  function(...) {
    Call <- match.call()
    Call[[1]] <- get_calling_name("prmt", as.character(Call[[1]]))
    if (is.null(ret <- eval(Call, envir = parent.frame())))
      return (invisible(.))
    ret
  }


function_... <- function(...) ..1

## Functions with any number of parameters. It is suitable for
## functions such as print() or View(), that return NULL.
trvs_function_. <-
  trvs_print <- 
  trvs_View <-

  function(...) {
    Call <- match.call(expand.dots = TRUE)
    Call[[1]] <- get_calling_name("trvs", as.character(Call[[1]]))
    if (is.null(ret <- eval(Call, envir = parent.frame())))
      return (invisible(..1))
    ret
  }



function_data_... <- function(data, ...) data

ntbt_function_data_... <-
  
  ## graphics
  ntbt_boxplot <-
  ntbt_cdplot <-
  ntbt_mosaicplot <-
  ntbt_pairs <-
  ntbt_plot <-
  ntbt_spineplot <-
  ntbt_sunflowerplot <-
  ntbt_text <-
  
  ## MASS
  ntbt_corresp <-
  ntbt_lda <-
  ntbt_qda <-
  ntbt_rlm <-
  
  ## nnet
  ntbt_nnet <-
  
  ## randomForest
  ntbt_randomForest <-
  
  ## stats
  ntbt_ansari.test <-
  ntbt_bartlett.test <-
  ntbt_cor.test <-
  ntbt_fligner.test <-
  ntbt_friedman.test <-
  ntbt_kruskal.test <-
  ntbt_lqs <-
  ntbt_mood.test <-
  ntbt_ppr <-
  ntbt_prcomp <-
  ntbt_princomp <-
  ntbt_quade.test <-
  ntbt_t.test <-
  ntbt_var.test <-
  ntbt_wilcox.test <-
  
  ## e1071
  ntbt_svm <-
  
  ## Ex no need to switch
  ## graphics
  ntbt_coplot <-
  
  ## lattice
  ntbt_oneway <-
  
  ## lfe
  ntbt_felm <-
  
  ## MASS
  ntbt_glm.nb <-
  ntbt_lm.gls <-
  ntbt_lm.ridge <-
  ntbt_loglm <-
  ntbt_polr <-
  
  ## nnet
  ntbt_multinom <-
  
  ## pls
  ntbt_cppls <-
  ntbt_mvr <-
  ntbt_pcr <-
  ntbt_plsr <-
  
  ## rpart
  ntbt_rpart <-
  
  ## stats
  ntbt_aov <-
  ntbt_ftable <-
  ntbt_lm <-
  ntbt_loess <-
  ntbt_model.frame <-
  ntbt_nls <-
  ntbt_oneway.test <-
  ntbt_replications <-
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
  
  ## MASS
  ntbt_logtrans <-
  
  ## nlme <-
  ntbt_lmList <-
  
  ## stats
  ntbt_alias <-
  ntbt_getInitial <-
  ntbt_model.matrix <-

  ## nlme
  ntbt_gls <-
  ntbt_nlme <-
  ntbt_nlsList <-

  ## nlme
  ntbt_lme <-
  
  ntbt_aggregate <- ## stats
  
  ntbt_gam <-    ## gam
  ntbt_glm <-  ## stats
  
  ntbt_gbm <-   ## gbm
  ##    I will try some cases and we will see.
  ##    This may be the definitive solution.
  
  ## The function below seems to address *all*
  ## that will feed on data, regardless
  ## of having formula or not. Moreover, it can
  ## be an excellent idea in some cases to start,
  ## instead than from a data.frame or tibble,
  ## from a list (or an environment) so the different
  ## functions can work with variables of different
  ## lengths if necessary.
  function(data, ...) {
    
    ## We need to make a special case with formula because if in
    ## the model there is a ".", the general approach below, after attaching,
    ## will fail as it does not know how to expand that "."
    ## This top part was, in a nutshel, intubate so far.
    
    Call <- match.call(expand.dots = FALSE)
    formula <- as.character(Call$...[[1]])
    
    ## It's my best effort, so far, in trying to determine if the first
    ## parameter after data is a formula.
    ## If there is a model, it better be right after data.
    if (formula[1] == "~" && length(formula) >= 2 && length(formula) <= 3) {
      ## message("Formula\n")
      Call <- match.call(expand.dots = TRUE)
      Call[[1]] <- get_calling_name("ntbt", as.character(Call[[1]]))
      print(Call[[1]])
      ## We switch data and model as almost all functions will be
      ## expecting first model and then data.
      Call[2:3] <- Call[3:2]                  ## Switching parameters
      ## We name data (all cases with formula have second parameter
      ## called data) and we remove the name of the formula, as there
      ## are variants: formula, object, x, model, and fixed.
      ## No function (I hope) will complain if it's unnamed if ordered.
      ## All the rest of parameters must be named (or in natural order).
      names(Call)[2:3] <- c("", "data")

      ## And now we relax while the experts do their magic...
      if (is.null(ret <- eval(Call, envir = parent.frame())))
        return (invisible(data))  ## If no value returned, forward data.
      return(ret)
    }

    ## message("No Formula\n")
    ## New part that should be good for everything (yes... keep dreaming...)
    ## with the exception of formulas with a ".", addressed above. If it
    ## weren't for that detail, what is below should have been enough
    ## for all cases.
    attach(data)     ## Careful! We are attaching.
    Call <- match.call(expand.dots = TRUE)
    ## We need to make sure will are able to detach(),
    ## so we use a try for each of the following two calls,
    ## that can fail either for error on name or bad call.
    Call[[1]] <- try(get_calling_name("ntbt", as.character(Call[[1]])),
                     silent = TRUE)
    if (class(Call[[1]]) == "try-error") {
      detach()            ## First detach
      stop(Call[[1]])     ## Then stop
    }
    ## First we make sure that we remove "data" from the call
    ## and then we let the experts take care of business.
    ret <- try(eval(Call[-2], envir = parent.frame()), silent = TRUE)
    detach()         ## First detach
    if (class(ret) == "try-error")
      stop(ret)      ## Then stop
    
    if (is.null(ret))
      return (invisible(data))  ## If no value returned, forward data.
    ret
  }



#library(magrittr)
#CO2 %>%
    #ntbt_lm(conc ~ Type)
#    ntbt_lm(conc ~ .)

## For now it fails only if you have a formula with a .
#function(., ...) {
#  attach(.)
#  Call <- match.call()
#  print(Call)
#  Call[[1]] <- get_calling_name("ntbt", as.character(Call[[1]]))
#  ret <- eval(Call[-2], envir = parent.frame())
#  detach()
#  if (is.null(ret))
#    return (invisible(.))
#  ret
#}
