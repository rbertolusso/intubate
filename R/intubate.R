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

  cfti <- all.names(Call[[3]])
  ## For now we ignore the namespace and call the function
  ## without it.
  cfti <- cfti[length(cfti)]
  ## For some reason (unknown to me), if we use instead the code
  #  if (length(cfti) == 3)
  #    cfti <- paste0(cfti[2], cfti[1], cfti[3])
  ## that would recreate the original code,
  ## as.name(cfti) expands to stats::lm enclosed in backsticks
  ## and I do not know for now how to get rid of those backsticks
  ## that still are there when you check the
  ## final Call (with print), and then
  ## there is an error when you eval().
  
  result <- process_call("ntbt", data, preCall, Call, cfti, parent.frame())
  
  if (result$result_visible)
    return (result$result)
  invisible(result$result)
}

## (external)
intuBag <- function(...) {
  iBag <- list(...)
  if (sum(names(iBag) == "") > 0)
    stop("All elements of an intuBag must be named.")
  ## class(iBag) <- c("intuBag")
  attr(iBag, "intuBag") <- TRUE
  iBag
}

## (external)
as_intuBag <- function(object) {
  if (sum(names(object) == "") > 0)
    stop("All elements of an intuBag must be named.")
  iBag <- as.list(object)
  ## class(iBag) <- c("intuBag")
  attr(iBag, "intuBag") <- TRUE
  iBag
}

## (external)
is_intuBag <- function(object) {
  ## sum(class(object) == "intuBag") > 0
  (!is.null(attr(object, "intuBag")) && attr(object, "intuBag"))
}

## (external)
intuEnv <- function() {
  local_env$intuEnv
}

## (external)
set_intuEnv <- function(envir) {
  old_intuEnv <- local_env$intuEnv
  if (!is.environment(envir))
    stop("You need to provide an environment!")
  local_env$intuEnv <- envir
  old_intuEnv
}


## Internal variables and functions

local_env <- new.env()          ## Local environment

## intuEnv
local_env$intuEnv <- new.env()
attr(local_env$intuEnv, "name") <- "intuEnv"

## (internal)
process_call <- function(called_from, data, preCall, Call, cfti, use_envir) {
  
  #if (io$be_verbose) {
    #if (io$show_diagnostics)
  print(Call)

  if (called_from == "ntbt")
    Call[[3]] <- NULL
  
  io <- parse_io(preCall$..., data)

  ## if (io$show_diagnostics) print(preCall)
  
  ##  print(io$found)
  if (io$found) {
    preCall$...[[io$pos]] <- NULL   ## Remove intubOrder
    Call[[io$pos + 2]] <- NULL
  }
  input_data <- io$input_data

  Call[[1]] <- as.name(cfti)
  
  if (io$show_diagnostics) {cat("* Function to call, with intubOrder removed:\n"); print(Call)}
  if (io$show_diagnostics) cat("* Formals:", names(formals(cfti)), "\n")
  
  #first_par_name <- names(formals(cfti))[1]
  
  errors <- list()
  #if (first_par_name %in% c("data", ".data", "_data")) { ## already pipe-aware function
  #  if (io$show_diagnostics) cat("* Already pipe-aware function\n")
  #  names(Call)[[2]] <- first_par_name
  #  if (io$show_diagnostics) print(Call)
  #  result <- eval(Call, envir = use_envir)
  #} else
  #if (length(preCall$...) == 0)  {
  #  if (io$show_diagnostics) cat("* No arguments other than data\n")
  #  if (io$show_diagnostics) print(Call)
  #  result <- eval(Call)
  #} else
  #if (there_are_formulas(preCall$...) || io$force_formula_case) {
  #  if (io$show_diagnostics) cat("* Formula case\n")
  #  if (io$input != "") {
  #    Call[[2]] <- as.name(io$input)
  #    which_envir <- input_data
  #  } else
  #    which_envir <- use_envir
  #  if (io$show_diagnostics) print(Call)
  #  ret <- process_formula_case(Call, which_envir, data, io, errors)
  #  result <- ret$result
  #  result_visible <- ret$result_visible
  #  Call <- ret$Call
  #} else  { ## Rest of cases
    if (io$input != "")
      which_input_data <- input_data[[io$input]]  ## Need to get the object inside the collection.
    else
      which_input_data <- input_data
    if (io$show_diagnostics) { cat("* Strategy # 1\n"); print(Call[-2]) }
    ## Remove "data" [-2] when calling
    result <- try(with(which_input_data, eval(Call[-2])), silent = TRUE)
    if (class(result)[[1]] == "try-error") {
      errors[[paste0("Error", length(errors) + 1)]] <-
        list(context = "Strategy # 1", call_attempted = Call[-2], error_message = result)
      if (io$input != "") {
        Call[[2]] <- as.name(io$input)
        which_envir <- input_data
      } else
        which_envir <- use_envir
      Call_data_unnamed <- Call               ## Create a copy for modification.
      names(Call_data_unnamed)[[2]] <- ""     ## Leave data unnamed.
      if (io$show_diagnostics) { cat("* Strategy # 2\n"); print(Call_data_unnamed) }
      result <- try(eval(Call_data_unnamed, envir = which_envir), silent = TRUE)
      if (class(result)[[1]] == "try-error") {
        errors[[paste0("Error", length(errors) + 1)]] <-
          list(context = "Strategy # 2", call_attempted = Call_data_unnamed, error_message = result)
        ## if (io$show_diagnostics) cat("* Calling formula case from Rest of cases\n")
        ret <- process_formula_case(Call, which_envir, data, io, errors)
        ## Try formula (formula could be result of a function call)
        result <- ret$result
        result_visible <- ret$result_visible
        Call <- ret$Call
      }
    } else {
      Call = Call[-2]
    }
  #}
  if (!exists("result_visible"))
    result_visible <- withVisible(result)$visible
  
  if (io$show_diagnostics || io$show_successful_call) { cat("* Successful call:\n"); print(Call) }
  if (io$show_diagnostics) cat(paste0("* Result is ", ifelse(result_visible, "", "in"), "visible\n"))
  if (io$show_diagnostics && is.null(result)) cat("* Result is null\n")
    
  if (io$found) {
    if (io$show_diagnostics && length(io$input_functions) > 0)
      { cat("* Input functions:\n"); print(io$input_functions) }
    exec_io(io$input_functions, "source", input_data, input_data, io)

    if (io$show_diagnostics && length(io$result_functions) > 0)
      { cat("* Result functions:\n"); print(io$result_functions) }
    exec_io(io$result_functions, "result", result, input_data, io)

    if (io$show_diagnostics && io$force_return_invisible) cat("* Force return invisible\n")
  }

  if (!is.null(result) && io$output != "") {
    if (io$is_intuBag) {
      data[[io$output]] <- result
      ##    data[io$output] <- ifelse(is.list(result), result, list(result)) ## For later
    } else {
      assign(io$output, result, envir = intuEnv())
    }
  }
  ##  print(io)
  if (!io$is_intuBag) {
    if (!is.null(result) && !io$forward_input) {
      if (result_visible && !io$force_return_invisible) {
        return (list(result = result, result_visible = TRUE))
      } else
        data <- result
    }
  }
  list(result = data, result_visible = FALSE)
}


## (internal)
parse_io <- function(par_list, data) {
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
  
  io$input_functions <- trimws(gsub(".*<([^|]*)\\|[^|]*\\|.*>.*", "\\1", io$intubOrder))
  io$input_functions <- trimws(strsplit(io$input_functions, ";")[[1]])

  io$result_functions <- trimws(gsub(".*<[^|]*\\|[^|]*\\|(.*)>.*", "\\1", io$intubOrder))
  io$result_functions <- trimws(strsplit(io$result_functions, ";")[[1]])

  io$show_successful_call <- (gsub(".*<.*\\|.*(C).*\\|.*>.*", "\\1", io$intubOrder) == "C")
  io$show_diagnostics <- (gsub(".*<.*\\|.*(D).*\\|.*>.*", "\\1", io$intubOrder) == "D")
  io$force_formula_case <- (gsub(".*<.*\\|.*(F).*\\|.*>.*", "\\1", io$intubOrder) == "F")
  io$forward_input <- (gsub(".*<.*\\|.*(f).*\\|.*>.*", "\\1", io$intubOrder) == "f")
  io$force_return_invisible <- (gsub(".*<.*\\|.*(i).*\\|.*>.*", "\\1", io$intubOrder) == "i")
  io$be_verbose <- (gsub(".*<.*\\|.*(v).*\\|.*>.*", "\\1", io$intubOrder) == "v")
  
  input_output <- strsplit(paste0(" ", io$intubOrder, " "), ## Spaces to avoid failure.
                           "<.*\\|.*\\|.*>")[[1]]
  if (length(input_output) > 2)             ## For overachievers...
    stop(paste0("Only one intuBorder, ", intuBorder, ", is currently implemented.\n"))

  ## Get requested inputs.
  ## cat("Inputs\n")
  io$is_intuBag <- is_intuBag(data)

  io$input <- trimws(input_output[1])
  if (io$input != "") {   ## Add requirement of data being an intuBag?
  ##  io$input_data <- as.list(data[io$input])  ## We removed class.
    if (is.environment(data)) {
      io$input_data <- list(get(io$input, envir = data))
      names(io$input_data) <- io$input
    } else {
      io$input_data <- data[io$input]
    }
  } else
    io$input_data <- data
  
  ## Get names of outputs.
  ## cat("Outputs\n")
  io$output <- trimws(input_output[2])

  #  print(io)
  io
}

## (internal)
exec_io <- function(..object_functions.., where, ..object_value.., ..envir.., ..io..) {
  for (this_function in ..object_functions..) {
    include_object <- TRUE
    if (this_function == "print") {
      printed <- capture.output(print(..object_value..))
    } else {
      if (length(strsplit(this_function, "\\(")[[1]]) > 1) {
        printed <- capture.output(print(eval(parse(text = gsub("#", "..object_value..", this_function)),
                                             envir = ..envir..)))
      } else {
        printed <- capture.output(print(do.call(this_function, args=list(quote(..object_value..)))))
      }
    }
    ## print(str(printed))
    if (length(printed) > 0 && printed[1] != "NULL" && include_object) {
      cat("\n")
      header <- paste0("* ", this_function, " <||> ", where, " *")
      sep <- paste0(paste0(rep("-", nchar(header)), collapse = ""), "\n")
      #cat(sep)
      if (..io..$be_verbose) cat(paste0(header, "\n"))
      #cat(sep)
      cat(printed[printed != "NULL"], sep = "\n")
    }
  }
}


## (internal)
## This special case for formulas is, at least for now, needed because
## "Rest of cases" below does not know how to manage cases with "." in
## a formula (and the called function neither because only sees the variables
## inside the data, not the data itself). An alternative could be to have in
## "Rest of cases" some sort of "stats::model.matrix" call (of which I have
## not clue how to implement), for *all* the formulas that have a ".", so maybe
## the end result will be even more involved and this is as good as we can do.
process_formula_case <- function(Call, use_envir, data, io, errors) {
  pos <- which(sapply(charCall <- as.character(Call), function(par) {
    gsub(".*(#).*", "\\1", par) == "#"
  }))
  if (length(pos) > 0) {
    if (io$show_diagnostics) cat("* Position specified\n")
    to_parse <- gsub("[\"']?#[\"']?", charCall[[2]], charCall[[pos]])
    .res_expr. <- eval(parse(text = to_parse), envir = use_envir)
    Call[[pos]] <- as.name(".res_expr.")
    Call <- Call[-2]
    if (io$show_diagnostics) { cat("* Formula with position\n"); print(Call) }
    result <- eval(Call)    ## If you specify position, you better know what you are doing.
    return(list(result = result,
                result_visible = withVisible(result)$visible,
                Call = Call))
  }

  if (io$show_diagnostics) { cat("* Formula # 1\n"); print(Call) }
  ## Try as it is (data is named)
  result <- try(eval(Call, envir = use_envir), silent = TRUE)
  if (class(result)[[1]] != "try-error") {
    return(list(result = result,
                result_visible = withVisible(result)$visible,
                Call = Call))
  }
  errors[[paste0("Error", length(errors) + 1)]] <-
    list(context = "Formula # 1", call_attempted = Call, error_message = result)
  
  Call[2:3] <- Call[3:2]                       ## Switch parameters
  names(Call)[2:3] <- names(Call)[3:2]         ## and names
  ## names(Call)[2:3] <- c("", "data")            ## Leave formula unnamed
  if (io$show_diagnostics) { cat("* Formula # 2\n"); print(Call) }
  result <- try(eval(Call, envir = use_envir), silent = TRUE)
  if (class(result)[[1]] != "try-error") {
    return(list(result = result,
                result_visible = withVisible(result)$visible,
                Call = Call))
  }
  errors[[paste0("Error", length(errors) + 1)]] <-
    list(context = "Formula # 2", call_attempted = Call, error_message = result)
  
  ## Maybe data has other name. Remove parameter name for "data"
  names(Call)[[3]] <- ""
  if (io$show_diagnostics) { cat("* Formula # 3\n"); print(Call) }
  result <- try(eval(Call, envir = use_envir), silent = TRUE)   ## Retry
  if (class(result)[[1]] != "try-error") {
    return(list(result = result,
                result_visible = withVisible(result)$visible,
                Call = Call))
  }
  errors[[paste0("Error", length(errors) + 1)]] <-
    list(context = "Formula # 3", call_attempted = Call, error_message = result)
  
  ## Maybe "data" position is still to the right
  if (length(Call) > 3) {     ## Are there more parameters to the right?
    ## Let's have "data" take a walk until it finds its place in the world,
    ## as functions are supposed to check if unnamed parameters are sent
    ## in the right order (you hope, at least).
    for (par in 4:length(Call)) {
      Call[(par-1):par] <- Call[par:(par-1)]        ## Switch parameters
      names(Call)[(par-1):par] <- names(Call)[par:(par-1)]  ## and names
      if (io$show_diagnostics) { cat("* Formula # 4\n"); print(Call) }
      result <- try(eval(Call, envir = use_envir),    ## See if it flies
                    silent = TRUE)
      if (class(result)[[1]] != "try-error") {
        return(list(result = result,
                    result_visible = withVisible(result)$visible,
                    Call = Call))
      }
      errors[[paste0("Error", length(errors) + 1)]] <-
        list(context = "Formula # 4", call_attempted = Call, error_message = result)
    }
  }

  ## Parameters exhausted and still error
  ## Try attaching data and call, It will fail if there is a . in the formula,
  ## but at this point there is no much else to do.
  ## This is needed to accomodate at least calibrate() in EnvStats,
  ## that seem to not accept "." as name of data when called in a pipeline
  ## (the interface called directly with the name of the data works fine).
  ## So let's make it believe we are just in some sort of "global environment"
  ## working with local variables. It would be better if the authors of EnvStats
  ## improve the data management.
  ## Remove "data" (now at the end of Call) then call.

  ## check --as-cran does not like the attach() below. Currently it is there
  ## only to address calibrate() in EnvStats. So for now, I will remove
  ## the interface to calibrate(), and get back to this after 0.99.3
  ## is submitted (maybe later this week).
  ##
  ## If we get to this point, we will just admit defeat.
  
#  Call <- Call[-length(Call)]
#  if (io$show_diagnostics) print(Call)
#  attach(data) ## Tried with() but calibrate() still complained. Too high maintenance!
#  result <- try(eval(Call)), silent = TRUE)  ## Use try as we use attach()
#  detach()
  if (class(result)[[1]] == "try-error") {
    cat("\n************\nintubate\nStart of error messages (last to first)\n:")
    print(errors[length(errors):1])
    stop(paste0("Message from intubate:\n",
                "All possibilities have been exhausted.\n",
                "The error may be due to intubate or to the interfaced function.\n",
                "If it is due to the interfaced function, you may find the cause\n",
                "in one of the errors listed, in reverse order, above.\n"))
  }
  list(result = result,
       result_visible = withVisible(result)$visible,
       Call = Call)
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
  cfti <- gsub(paste0(prefix, "_(.+)"), "\\1", full_name)
  if (!exists(cfti)) {
    stop(paste0("The function <| ", cfti, " |> does not seem to be defined.\n",
                "Did you install the package where <| ", cfti, " |> is included?\n",
                "If not, please run: install.packages(\"package_name\")\n",
                "Did you load the corresponding library?\n",
                "If not, please run: library(package_name)\n",
                "To keep system resources low, intubate does not install, nor loads, packages."))
  }
  cfti
}


#intubate_interfaces <- function() {
#  ls("package:intubate")
#  unlockBinding("intuBag", as.environment("package:intubate"))
#  rm(intuBag, envir = as.environment("package:intubate"))
#  ls("package:intubate")
#}
