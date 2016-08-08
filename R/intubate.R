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

    if (withVisible(result)$visible)
      return (result)
    invisible(result)
  }

## (external)
function(data, ...) {
    preCall <- match.call(expand.dots = FALSE)
    
    Call <- match.call(expand.dots = TRUE)
    Call[[length(Call) + 1]] <- ""
    for (pos in length(Call):3) {
      Call[[pos]] <- Call[[pos - 1]]
      names(Call)[[pos]] <- names(Call)[[pos - 1]]
    }      
    Call[[3]] <- get_calling_name("ntbt", as.character(Call[[1]]))
    names(Call)[[3]] <- "fti"
    Call[[length(Call) + 1]] <- parent.frame()
    names(Call)[[length(Call)]] <- "use_envir" 
    Call[[1]] <- quote(intubate::ntbt)

    print(Call)
    eval(Call)
  }

## (external)
ntbt <- function(data, fti, ...) {
  preCall <- match.call(expand.dots = FALSE)
  Call <- match.call(expand.dots = TRUE)

  fti_name <- all.names(Call[[3]])
  ## For now we ignore the namespace and call the function
  ## without it.
  fti_name <- fti_name[length(fti_name)]
  ## For some reason (unknown to me), if we use instead the code
  #  if (length(fti_name) == 3)
  #    fti_name <- paste0(fti_name[2], fti_name[1], fti_name[3])
  ## that would recreate the original code,
  ## as.name(fti_name) expands to ´stats::lm´ and I do not know
  ## for now how to get rid of those ´´ that still are there
  ## if you check your final Call (with print), and then
  ## there is an error when you eval().
  
  Call[[1]] <- as.name(fti_name)
  Call[[3]] <- NULL

  result <- process_call(data, preCall, Call, parent.frame())

  if (withVisible(result)$visible)
    return (result)
  invisible(result)
}


## (internal)
function(data, fti, ..., use_envir = parent.frame()) {
  preCall <- match.call(expand.dots = FALSE)
  Call <- match.call(expand.dots = TRUE)
  Call[[length(Call)]] <- NULL
  
  Call[[1]] <- as.name(fti)
  Call[[3]] <- NULL
  
  io <- parse_intubOrder(preCall$...)
  if (io$found) {
    preCall$...[[io$pos]] <- NULL
    Call[[io$pos + 2]] <- NULL
  }
  
  if (length(preCall$...) == 0)  {
    ## cat("No arguments other than data\n")
    ## print(Call)
    result <- eval(Call)
  } else if (there_are_formulas(preCall$...) || io$use_formula_case) {
    ## cat("Formula\n")
    result <- process_formula_case(Call, use_envir = use_envir)
  } else  {
    ## cat("Rest of cases\n")
    ## print(Call)
    result <- with(data, eval(Call[-2]))    ## Remove "data" [-2] before calling
  }
  
  if (io$found) {
    if (io$print_result)
      print(result)
    if (io$print_summary_result)
      print(summary(result))
    if (io$plot_result)
      plot(result)
  }
  
  if (!is.null(result) && !io$forward_input) {
    if (withVisible(result)$visible)
      return (result)
    else
      data <- result
  }
  invisible(data)
}

## (internal)
process_call <- function(data, preCall, Call, use_envir) {
##  print(Call)
  io <- parse_intubOrder(preCall$..., data)
##  print(io$found)
  if (io$found) {
    preCall$...[[io$pos]] <- NULL   ## Remove intubOrder
    Call[[io$pos + 2]] <- NULL
  }
  input_data <- io$input_data
##  print(Call)
  
  if (length(preCall$...) == 0)  {
    cat("No arguments other than data\n")
    print(Call)
    result <- eval(Call)
  } else if (there_are_formulas(preCall$...) || io$use_formula_case) {
    cat("Formula\n")
    if (io$is_intuBag)
      Call[[2]] <- as.name(io$input[1])
    print(Call)
    result <- process_formula_case_1(Call, use_envir)      
  } else  {
    cat("Rest of cases\n")
    print(Call)
    if(length(input_data) == 1 && !is_intuBag(input_data))
      input_data <- input_data[[1]]  ## Need to get the object inside the list.
    result <- try(with(input_data, eval(Call[-2])), silent = TRUE) ## Remove "data" [-2] then call
    if (class(result)[[1]] == "try-error") {
      if (io$is_intuBag)
        Call[[2]] <- as.name(io$input[1])
      names(Call)[[2]] <- ""                     ## Leave data unnamed
      print(Call)
      result <- eval(Call) ## For subset() and such, that already are
                           ## pipe aware.
    }
  }

  result_visible <- withVisible(result)$visible

  exec_intubOrder(io, result)

  if (!is.null(result) && io$output[1] != "")
    data[[io$output[1]]] <- result
#    data[io$output] <- ifelse(is.list(result), result, list(result)) ## For later
    
##  print(io)
  if (!io$is_intuBag) {
    if (!is.null(result) && !io$forward_input) {
      if (result_visible)
        return (result)
      else
        data <- result
    }
  }
  invisible(data)
}

## (internal)
parse_intubOrder <- function(par_list, data) {
  intuBorder <- "<||>"
  io <- list()
  io$found <- FALSE
  for (pos in 1:length(par_list)) {
    io$intubOrder <- par_list[[pos]]
    if (is.character(io$intubOrder) &&
        gsub(".*(<).*(\\|).*(\\|).*(>).*", "\\1\\2\\3\\4", io$intubOrder)[[1]] == intuBorder) {
      io$found <- TRUE
      io$pos <- pos
      break
    }
  }
  if (!io$found)
    io$intubOrder <- intuBorder
  
  io$print_result <- (gsub(".*<.*\\|.*\\|.*(p).*>.*", "\\1", io$intubOrder) == "p")
  io$plot_result <- (gsub(".*<.*\\|.*\\|.*(P).*>.*", "\\1", io$intubOrder) == "P")
  io$print_summary_result <- (gsub(".*<.*\\|.*\\|.*(s).*>.*", "\\1", io$intubOrder) == "s")
  io$use_formula_case <- (gsub(".*<.*\\|.*\\|.*(F).*>.*", "\\1", io$intubOrder) == "F")
  io$forward_input <- (gsub(".*<.*\\|.*\\|.*(f).*>.*", "\\1", io$intubOrder) == "f")
  
  io$is_intuBag <- is_intuBag(data)
  input_output <- strsplit(paste0(" ", io$intubOrder, " "), ## Spaces to avoid failure.
                           "<.*\\|.*\\|.*>")[[1]]
  if (length(input_output) > 2)             ## For overachievers...
    stop(paste0("Only one intuBorder, ", intuBorder, ", is currently implemented.\n"))

  ## Get requested inputs.
  ## cat("Inputs\n")
  io$input <- trimws(strsplit(input_output[1], ";")[[1]])
  
  if (io$input[1] != "") {
    io$input_data <- as.list(data[io$input])
  } else
    io$input_data <- data
  
  ## Get names of outputs.
  ## cat("Outputs\n")
  io$output <- trimws(strsplit(input_output[2], ";")[[1]])
  
#  print(io)
  io
}

## (internal)
exec_intubOrder <- function(io, result) {
  if (!io$found)
    return (FALSE)
  
  if (io$print_result) {
    cat("\n# -------------------\n")
    cat("intubate <||> print\n")
    print(result)
  }
  if (io$print_summary_result) {
    cat("\n# -------------------\n")
    cat("intubate <||> summary\n")
    print(summary(result))
  }
  if (io$plot_result)
    plot(result)
}

## (internal)
intuBag <- function(...) {
  iBag <- list(...)
  if (sum(names(iBag) == "") > 0)
    stop("All elements of an intuBag must be named.")
  class(iBag) <- c("intuBag")
  iBag
}

## (internal)
is_intuBag <- function(object) {
  sum(class(object) == "intuBag") > 0
}

## (internal)
## This special case for formulas is, at least for now, needed because
## "Rest of cases" below does not know how to manage cases with "." in
## a formula (and the called function neither because only sees the variables
## inside the data, not the data itself). An alternative could be to have in
## "Rest of cases" some sort of "stats::model.matrix" call (of which I have
## not clue how to implement), for *all* the formulas that have a ".", so maybe
## the end result will be even more involved and this is as good as we can do.
process_formula_case_1 <- function(Call, use_envir) {
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

## (internal)
## This special case for formulas is, at least for now, needed because
## "Rest of cases" below does not know how to manage cases with "." in
## a formula (and the called function neither because only sees the variables
## inside the data, not the data itself). An alternative could be to have in
## "Rest of cases" some sort of "stats::model.matrix" call (of which I have
## not clue how to implement), for *all* the formulas that have a ".", so maybe
## the end result will be even more involved and this is as good as we can do.
process_formula_case_2 <- function(Call, use_envir) {
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

## (internal)
## This special case for formulas is, at least for now, needed because
## "Rest of cases" below does not know how to manage cases with "." in
## a formula (and the called function neither because only sees the variables
## inside the data, not the data itself). An alternative could be to have in
## "Rest of cases" some sort of "stats::model.matrix" call (of which I have
## not clue how to implement), for *all* the formulas that have a ".", so maybe
## the end result will be even more involved and this is as good as we can do.
process_formula_case_3 <- function(Call) {
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


## (internal)
## Determine if there is a formula
there_are_formulas <- function(par_list) {
  sum(sapply(par_list,
             function(par) {
               is_formula <- try(inherits(as.formula(as.character(par)),
                                          "formula"), silent = TRUE)
               if (class(is_formula)[[1]] == "try-error")
                 return (FALSE)
               as.character(par)[[1]] == "~"
             }) > 0)
}

## (internal)
## Determine if there is a formula
function(par_list) {
  sum(sapply(par_list,
             function(par) {
               is_formula <- try(inherits(as.formula(as.character(par)),
                                          "formula"), silent = TRUE)
               !(class(is_formula)[[1]] == "try-error")
             }) > 0)
}


## (internal)
## Name checking
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
