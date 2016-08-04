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

## Interface function
ntbt_function_data <- function(data, ...) {
  preCall <- match.call(expand.dots = FALSE)
  
  Call <- match.call(expand.dots = TRUE)
  Call[[1]] <- get_calling_name("ntbt", as.character(Call[[1]]))
  Call[[2]] <- as.name("data")
  
  is_formula <- try(inherits(as.formula(as.character(preCall$...[[1]])),
                             "formula"), silent = TRUE)
  is_formula <- !(class(is_formula)[[1]] == "try-error")
  
  if (is_formula) {
    ## cat("Formula\n")
    Call[2:3] <- Call[3:2]                ## Switch parameters
    names(Call)[2:3] <- c("", "data")     ## Leave formula unnamed
    print(Call)
    ret <- try(eval(Call), silent = TRUE) ## Let's make it more robust
    if (class(ret)[[1]] == "try-error") { ## Maybe data has other name
      names(Call)[[3]] <- ""              ## Leave data unnamed
      print(Call)
      ret <- eval(Call)                   ## Retry
    }
  } else if (length(preCall$...) > 0)  {
    ## cat("Rest of cases\n")
    print(Call)
    ret <- with(data, eval(Call[-2]))     ## Need to remove data [-2]
  } else  {
    ## cat("No arguments other that data\n")
    names(Call)[[2]] <- ""                ## Leave data unnamed
    print(Call)
    ret <- eval(Call)
  }
  if (!is.null(ret)) {
    if (withVisible(ret)$visible)
      return (ret)
    else
      data <- ret
  }
  invisible(data)
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
