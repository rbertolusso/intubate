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

## (external)
intuBag <- function(...) {
  iBag <- list(...)
  if (sum(names(iBag) == "") > 0)
    stop("All elements of an intuBag must be named.")
  class(iBag) <- c("intuBag")
  iBag
}

## (external)
is_intuBag <- function(object) {
  sum(class(object) == "intuBag") > 0
}

## Internal functions

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
    result <- process_formula_case(Call, use_envir)      
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
