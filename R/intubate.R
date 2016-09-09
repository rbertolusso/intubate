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

  check_cfti(cfti)

  result <- process_call("ntbt", data, preCall, Call, cfti, parent.frame())

  if (result$result_visible)
    return (result$result)
  invisible(result$result)
}


## (external)
intubate_rm_all_interfaces <- function() {
  ntbt_env <- as.environment("package:intubate")
  fn <- ls(ntbt_env)
  ndx <- which(gsub("(ntbt_).+", "\\1", fn) == "ntbt_")
  if (ret <- (length(ndx) > 0)) {
    ntbt_ns <- asNamespace("intubate")
    .Call("unlock_envir", ntbt_ns)
    .Call("unlock_envir", ntbt_env)
    rm(list = fn[ndx], envir = ntbt_ns)
    rm(list = fn[ndx], envir = ntbt_env)
    lockEnvironment(ntbt_env, bindings = TRUE)
    lockEnvironment(ntbt_ns, bindings = TRUE)
  }
  invisible(ret)
}


## (external)
intuBag <- function(...) {
  iBag <- list(...)
  if (sum(names(iBag) == "") > 0)
    stop("All elements of an intuBag must be named.")
  ## class(iBag) <- c("intuBag")
  attr(iBag, "intuBag") <- TRUE

  invisible(iBag)
}

## (external)
as_intuBag <- function(object) {
  if (sum(names(object) == "") > 0)
    stop("All elements of an intuBag must be named.")
  iBag <- as.list(object)
  ## class(iBag) <- c("intuBag")
  attr(iBag, "intuBag") <- TRUE

  invisible(iBag)
}

## (external)
is_intuBag <- function(object) {
  ## sum(class(object) == "intuBag") > 0
  (!is.null(attr(object, "intuBag")) && attr(object, "intuBag"))
}

## (external)
intuEnv <- function(...) {
  object_list <- list(...)
  if (length(object_list) > 0) {
    nms <- names(object_list)
    if (sum(names(nms) == "") > 0)
      stop("All elements in intuEnv must be named.")
    
    for (i in 1:length(nms))
      assign(nms[i], object_list[[i]], envir = local_env$intuEnv)
  }
  
  invisible(local_env$intuEnv)
}

## (external)
set_intuEnv <- function(envir) {
  old_intuEnv <- local_env$intuEnv
  if (!is.environment(envir))
    stop("You need to provide an environment!")
  local_env$intuEnv <- envir

  invisible(old_intuEnv)
}

## (external)
clear_intuEnv <- function() {
  rm(list = ls(intuEnv()), envir = intuEnv())
}

## Internal variables and functions

local_env <- new.env()          ## Local environment

## intuEnv
local_env$intuEnv <- new.env()
attr(local_env$intuEnv, "name") <- "intuEnv"

## (internal)
process_call <- function(called_from, data, preCall, Call, cfti, use_envir) {
  
  user_Call <- Call

  if (called_from == "ntbt")
    Call[[3]] <- NULL
  
  io <- parse_io(preCall$..., data)

  ## io$show_diagnostics <- TRUE

  if (io$found || io$show_diagnostics) {
    if (io$show_intubOrder)
        user_Call[[io$pos + ifelse(called_from == "ntbt", 3, 2)]] <- io$intubOrder
    else
      user_Call[[io$pos + ifelse(called_from == "ntbt", 3, 2)]] <- NULL
    if(length(io$input_functions) + length(io$result_functions) > 0) {
      cat("\n")
      print(user_Call)
    }
  }

  if (io$found) {
    preCall$...[[io$pos]] <- NULL   ## Remove intubOrder
    Call[[io$pos + 2]] <- NULL
  }
  input_data <- io$input_data

  Call[[1]] <- as.name(cfti)

  if (io$show_diagnostics) { cat("* Function to call, with intubOrder removed:\n"); print(Call) }
  if (io$show_diagnostics) cat("* Formals:", names(formals(cfti)), "\n")

  ret <- call_interfaced_function(cfti, Call, use_envir, input_data, io)
  result <- ret$result
  result_visible <- ret$result_visible
  Call <- ret$Call
  
  if (io$show_diagnostics || io$show_successful_call) { cat("* Successful call:\n"); print(Call) }
  if (io$show_diagnostics) cat(paste0("* Result is ", ifelse(result_visible, "", "in"), "visible\n"))
  if (io$show_diagnostics && is.null(result)) cat("* Result is null\n")
    
  if (length(io$input_functions) > 0) {
    if (io$show_diagnostics) { cat("* Input functions:\n"); print(io$input_functions) }
    if (io$input != "") {
      which_input_name <- io$input
      which_input_data <- input_data[[io$input]]
    } else {
      which_input_name <- "input"
      which_input_data <- input_data
    }
    data <- exec_io(io$input_functions, which_input_name,
                    which_input_data, io, data)
  }
  if (length(io$result_functions) > 0) {
    if (io$result != "")
      which_result_name <- io$result
    else
      which_result_name <- "result"
    if (io$show_diagnostics) { cat("* Result functions:\n"); print(io$result_functions) }
    data <- exec_io(io$result_functions, which_result_name, result, io, data)
  }

  if (io$show_diagnostics && io$force_return_invisible) cat("* Force return invisible\n")

  if (!is.null(result) && io$result != "") {
    if (io$is_intuBag) {
      data[[io$result]] <- result
      ##    data[io$result] <- ifelse(is.list(result), result, list(result)) ## For later
    } else {
      assign(io$result, result, envir = intuEnv())
    }
  }

  if (!io$is_intuBag && !io$is_environment) {
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
        gsub("[^<]*(<).*(\\|).*(\\|).*(>).*", "\\1\\2\\3\\4", io$intubOrder)[[1]] == intuBorder) {
      io$found <- TRUE
      io$pos <- pos
      break
    }
  }
  if (!io$found)
    io$intubOrder <- intuBorder

  ## Remove extra spaces or newlines to better print the call  
  io$intubOrder <- gsub("[[:space:]]+", " ", io$intubOrder)
  
  io$input_functions <- trimws(gsub("[^<]*<([^|]*)\\|[^|]*\\|.*>.*", "\\1", io$intubOrder))
  io$input_functions <- trimws(strsplit(io$input_functions, ";")[[1]])

  io$result_functions <- trimws(gsub("[^<]*<[^|]*\\|[^|]*\\|(.*)>.*", "\\1", io$intubOrder))
  io$result_functions <- trimws(strsplit(io$result_functions, ";")[[1]])

  io$show_successful_call <- (gsub("[^<]*<.*\\|.*(C).*\\|.*>.*", "\\1", io$intubOrder) == "C")
  io$show_diagnostics <- (gsub("[^<]*<.*\\|.*(D).*\\|.*>.*", "\\1", io$intubOrder) == "D")
  io$force_formula_case <- (gsub("[^<]*<.*\\|.*(F).*\\|.*>.*", "\\1", io$intubOrder) == "F")
  io$forward_input <- (gsub("[^<]*<.*\\|.*(f).*\\|.*>.*", "\\1", io$intubOrder) == "f")
  io$show_intubOrder <- (gsub("[^<]*<.*\\|.*(I).*\\|.*>.*", "\\1", io$intubOrder) == "I")
  io$force_return_invisible <- (gsub("[^<]*<.*\\|.*(i).*\\|.*>.*", "\\1", io$intubOrder) == "i")
  io$silent_on_assignment <- (gsub("[^<]*<.*\\|.*(S).*\\|.*>.*", "\\1", io$intubOrder) == "S")
  io$be_verbose <- (gsub("[^<]*<.*\\|.*(v).*\\|.*>.*", "\\1", io$intubOrder) == "v")
  
  input_result <- strsplit(paste0(" ", io$intubOrder, " "), ## Spaces to avoid failure.
                           "<.*\\|.*\\|.*>")[[1]]
  if (length(input_result) > 2)             ## For overachievers...
    stop(paste0("Only one intuBorder, ", intuBorder, ", is currently implemented.\n"))

  io$is_intuBag <- is_intuBag(data)
  io$is_environment <- is.environment(data)
  
  ## Get requested input.
  io$input <- trimws(input_result[1])
  if (io$input != "") {
    if (io$is_environment) {
      io$input_data <- list(get(io$input, envir = data))
      names(io$input_data) <- io$input
    } else {
      io$input_data <- data[io$input]
    }
  } else
    io$input_data <- data
  
  ## Get name of result.
  io$result <- trimws(input_result[2])

  io
}

## (internal)
exec_io <- function(object_functions, where, object_value, io, data) {
  for (this_fn in object_functions) {
    if (length(strsplit(this_fn, "\\(")[[1]]) == 1)
      this_fn <- paste0(this_fn, "(#)")
    this_fn_prnt <- this_fn
    
    this_fn <- gsub("#", "object_value", this_fn)
    
    this_fn_sp <- trimws(strsplit(this_fn, "<-")[[1]])
    
    printed <- capture.output(result <- eval(parse(text = this_fn_sp[length(this_fn_sp)]),
                                             envir = list(object_value = object_value)))
    if (!(length(printed) > 0 && printed[1] != "NULL") & !is.null(result))
      printed <- capture.output(print(result))
    
    if (length(this_fn_sp) > 1) {
      if (!is.null(result) && this_fn_sp[1] != "") {
        if (io$silent_on_assignment)
          printed <- character()
        if (io$is_intuBag) {
          data[[this_fn_sp[1]]] <- result
        } else {
          assign(this_fn_sp[1], result, envir = intuEnv())
        } 
      }
    }

    if (length(printed) > 0 && printed[1] != "NULL") {
      cat("\n")
      header <- paste0("* ", this_fn_prnt, " <||> ", where, " *")
      #sep <- paste0(paste0(rep("-", nchar(header)), collapse = ""), "\n")
      #cat(sep)
      cat(paste0(header, "\n"))
      #cat(sep)
      cat(printed[printed != "NULL"], sep = "\n")
    }
  }
  data
}


## (internal)
call_interfaced_function <- function(cfti, Call, use_envir, input_data, io) {
  
  pos <- which(sapply(charCall <- as.character(Call), function(par) {
    gsub(".*(#).*", "\\1", par) == "#"
  }))
  if (length(pos) > 0) {
    if (io$show_diagnostics) cat("* Position specified\n")
    to_parse <- gsub("[\"']?#[\"']?", charCall[[2]], charCall[[pos]])
    .res_expr. <- eval(parse(text = to_parse), envir = use_envir)
    Call[[pos]] <- as.name(".res_expr.")
    Call <- Call[-2]
    if (io$show_diagnostics) { cat("* Position specified\n"); print(Call) }
    result <- eval(Call)    ## If you specify position, you better know what you are doing.
    return(list(result = result,
                result_visible = withVisible(result)$visible,
                Call = Call))
  }

  Call_saved <- Call
  errors <- list()

  names_from_formal <- names(formals(cfti))
  data_pos <- which(names_from_formal %in% "data")
  if (length(data_pos) > 0 && data_pos > 2) {
    # names(Call)[[2]] <- names_from_formal[data_pos]
    data_pos <- data_pos + 1                          ## Adapt to our Call

    if (length(Call) > 2) {
      for (par in 3:min(data_pos, length(Call))) {    ## Move to natural position
        Call[(par-1):par] <- Call[par:(par-1)]        ## Switch parameters
        names(Call)[(par-1):par] <- names(Call)[par:(par-1)]  ## and names
      }
    }
    if (io$input != "") {
      Call[[data_pos]] <- as.name(io$input)
      which_envir <- input_data
    } else
      which_envir <- use_envir

    if (io$show_diagnostics) { cat("* Re-position data\n"); print(Call) }
    ## Try as it is (data is named)
    result <- try(eval(Call, envir = which_envir), silent = TRUE)
    if (class(result)[[1]] != "try-error") {
      return(list(result = result,
                  result_visible = withVisible(result)$visible,
                  Call = Call))
    }
    errors[[paste0("Error", length(errors) + 1)]] <-
      list(context = "Re-position data", call_attempted = Call, error_message = result)
    Call <- Call_saved
  }

  if (io$input != "")
    which_input_data <- input_data[[io$input]]  ## Need to get the object inside the collection.
  else
    which_input_data <- input_data

  Call <- Call[-2]
  if (io$show_diagnostics) { cat("* Using which_input_data as environment\n"); print(Call) }
##  result <- try(with(which_input_data, eval(Call)), silent = TRUE)
  result <- try(eval(Call, envir = which_input_data), silent = TRUE)
  if (class(result)[[1]] != "try-error") {
    return(list(result = result,
                result_visible = withVisible(result)$visible,
                Call = Call))
  }
  errors[[paste0("Error", length(errors) + 1)]] <-
    list(context = "Using which_input_data as environment", call_attempted = Call, error_message = result)
  Call <- Call_saved
  
  if (io$input != "") {
    Call[[2]] <- as.name(io$input)
    which_envir <- input_data
  } else
    which_envir <- use_envir
  names(Call)[[2]] <- ""     ## Leave data unnamed.
  if (io$show_diagnostics) { cat("* Leaving data unnamed\n"); print(Call) }
  result <- try(eval(Call, envir = which_envir), silent = TRUE)
  if (class(result)[[1]] != "try-error") {
    return(list(result = result,
                result_visible = withVisible(result)$visible,
                Call = Call))
  }
  errors[[paste0("Error", length(errors) + 1)]] <-
    list(context = "Leaving data unnamed", call_attempted = Call, error_message = result)
  Call <- Call_saved

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
  
  if (io$input != "") {
    Call[[2]] <- as.name(io$input)
    which_envir <- input_data
  } else
    which_envir <- use_envir
  
  Call[2:3] <- Call[3:2]                       ## Switch parameters
  names(Call)[2:3] <- names(Call)[3:2]         ## and names
  ## names(Call)[2:3] <- c("", "data")            ## Leave formula unnamed
  if (io$show_diagnostics) { cat("* Formula # 2\n"); print(Call) }
  result <- try(eval(Call, envir = which_envir), silent = TRUE)
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
  result <- try(eval(Call, envir = which_envir), silent = TRUE)   ## Retry
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
      result <- try(eval(Call, envir = which_envir),    ## See if it flies
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

#  Call <- Call[-length(Call)]
#  if (io$show_diagnostics) print(Call)
#  attach(input_data) ## Tried with() but calibrate() still complained. Too high maintenance!
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
  
  check_cfti(cfti)

  cfti
}

check_cfti <- function(cfti) {
  if (!exists(cfti)) {
    stop(paste0("The function <| ", cfti, " |> does not seem to exist.\n",
                "Did you install the package where <| ", cfti, " |> is included?\n",
                "If not, please run: install.packages(\"package_name\")\n",
                "Did you load the corresponding library?\n",
                "If not, please run: library(package_name)\n",
                "To keep system resources low, intubate does not install, nor loads, packages."))
  }
}
