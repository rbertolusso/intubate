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
  ## as.name(fti_name) expands to stats::lm enclosed in backsticks
  ## and I do not know for now how to get rid of those backsticks
  ## that still are there when you check the
  ## final Call (with print), and then
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

  if (length(preCall$...) == 0)  {
    #cat("No arguments other than data\n")
    #print(Call)
    result <- eval(Call)
  } else if (there_are_formulas(preCall$...) || io$force_formula_case) {
    #cat("Formula\n")
    if (io$is_intuBag)
      Call[[2]] <- as.name(io$input[1])
    #print(Call)
    ret <- process_formula_case(Call, use_envir)      
    result <- ret$result
    Call <- ret$Call
  } else  {
    #cat("Rest of cases\n")
    #print(Call)
    if(length(input_data) == 1 && !is_intuBag(input_data))
      input_data <- input_data[[1]]  ## Need to get the object inside the list.
    result <- try(with(input_data, eval(Call[-2])), silent = TRUE) ## Remove "data" [-2] then call
    if (class(result)[[1]] == "try-error") {
      if (io$is_intuBag)
        Call[[2]] <- as.name(io$input[1])
      names(Call)[[2]] <- ""                     ## Leave data unnamed
      #print(Call)
      result <- try(eval(Call), silent = TRUE) ## For subset() and such, that already are
                                               ## pipe aware.
      if (class(result)[[1]] == "try-error") {
        ret <- process_formula_case(Call, use_envir) ## Try formula (formula could be
                                                     ## result of a function call)
        result <- ret$result
        Call <- ret$Call
      }
    } else {
      Call = Call[-2]
    }
  }

  result_visible <- withVisible(result)$visible

  if (io$found) {
    cat("\n") 
    print(Call)
    exec_intubOrder(io$input_functions, "source", input_data)
    exec_intubOrder(io$result_functions, "result", result)
  }

  if (!is.null(result) && io$output[1] != "")
    data[[io$output[1]]] <- result
##    data[io$output] <- ifelse(is.list(result), result, list(result)) ## For later
    
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
  
  io$input_functions <- gsub(".*<(.*)\\|.*\\|.*>.*", "\\1", io$intubOrder)
  io$input_functions <- trimws(strsplit(io$input_functions, ";")[[1]])

  io$result_functions <- gsub(".*<.*\\|.*\\|(.*)>.*", "\\1", io$intubOrder)
  io$result_functions <- trimws(strsplit(io$result_functions, ";")[[1]])

  io$force_formula_case <- (gsub(".*<.*\\|.*(F).*\\|.*>.*", "\\1",
                                 io$intubOrder) == "F")
  io$forward_input <- (gsub(".*<.*\\|.*(f).*\\|.*>.*", "\\1",
                            io$intubOrder) == "f")
  io$invisible_result <- (gsub(".*<.*\\|.*(i).*\\|.*>.*", "\\1",
                               io$intubOrder) == "i")
  io$verbose <- (gsub(".*<.*\\|.*(v).*\\|.*>.*", "\\1",
                      io$intubOrder) == "v")
  
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
exec_intubOrder <- function(object_functions, where, object) {
  oldmfrow <- par()$mfrow    ## Just in case

  for (this_function in object_functions) {
    include_object <- TRUE
    if (this_function == "print") {
      printed <- capture.output(print(object))
    } else {
      if (length(strsplit(this_function, "\\(")[[1]]) > 1) {
        printed <- capture.output(print(eval(parse(text = gsub("#", "object", this_function)))))
      } else {
        printed <- capture.output(print(do.call(this_function, args=list(quote(object)))))
      }
    }
    ## print(str(printed))
    if (printed[1] != "NULL" && include_object) {
      cat("\n")
      header <- paste0("* ", this_function, " <||> ", where, " *")
      sep <- paste0(paste0(rep("-", nchar(header)), collapse = ""), "\n")
      #cat(sep)
      cat(paste0(header, "\n"))
      #cat(sep)
      cat(printed[printed != "NULL"], sep = "\n")
    }
  }
  par(mfrow = oldmfrow)
}


function(object_functions, where, object) {
  oldmfrow <- par()$mfrow    ## Just in case
  
  for (this_function in object_functions) {
    include_object <- TRUE
    if (this_function == "print") {
      printed <- capture.output(print(object))
    } else {
      if (length(strsplit(this_function, "\\(")[[1]]) > 1) {
        fun_name <- gsub("(.*)\\(.*\\).*", "\\1", this_function)
        
        if (gsub("(-).*", "\\1", fun_name) == "-") {
          fun_name <- gsub("-(.*)", "\\1", fun_name)
          include_object <- FALSE
        }
        fun_par <- gsub(".*\\((.*)\\).*", "\\1", this_function)
        
        strCall <- paste0(fun_name, "(", ifelse(include_object,
                                                "object, ",
                                                ""), fun_par, ")")
        printed <- capture.output(print(eval(parse(text = strCall))))
      } else {
        printed <- capture.output(print(do.call(this_function, args=list(quote(object)))))
      }
    }
    ## print(str(printed))
    if (printed[1] != "NULL" && include_object) {
      cat("\n")
      header <- paste0(" ", this_function, " <||> ", where, " ")
      sep <- paste0("# ", paste0(rep("-", nchar(header)), collapse = ""), "\n")
      cat(sep)
      cat(paste0("# ", header, "\n"))
      cat(sep)
      cat(printed[printed != "NULL"], sep = "\n")
    }
  }
  par(mfrow = oldmfrow)
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
  ## cat("process_formula_case\n")
  pos <- which(sapply(charCall <- as.character(Call), function(par) {
    gsub(".*(#).*", "\\1", par) == "#"
  }))
  if (length(pos) > 0) {
    to_parse <- gsub("[\"']?#[\"']?", charCall[[2]], charCall[[pos]])
    .res_expr. <- eval(parse(text = to_parse), envir = use_envir)
    Call[[pos]] <- as.name(".res_expr.")
    Call <- Call[-2]
    result <- try(eval(Call), silent = TRUE)
  } else {
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
  }
  list(result = result, Call = Call)
}


function(Call, use_envir) {
  cat("process_formula_case\n")
  pos <- which(sapply(charCall <- as.character(Call), function(par) {
    gsub(".*(<\\|\\|>).*", "\\1", par) == "<||>"
  }))
  if (length(pos) > 0) {
    charCall[[pos]] <- gsub("[\"']<\\|\\|>[\"']", charCall[[2]], charCall[[pos]])
    . <- eval(parse(text = charCall[[pos]]), envir = use_envir)
    Call[[pos]] <- quote(.)
    ## Call[[pos]] <- eval(parse(text = charCall[[pos]]), envir = use_envir)
    ##    Call[[pos]] <- eval(parse(text = charCall[[pos]]), envir = Call[-2])
    Call <- Call[-2]
    #charCall <- charCall[-2]
    print(Call)
    #print(names(charCall))
    #print(charCall)
    #Call <- Call[-2]
    
    #    print(eval(charCall))
  }
  #  print(Call[[5]])
  #  print(pos)
  
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
  list(result = result, Call = Call)
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
