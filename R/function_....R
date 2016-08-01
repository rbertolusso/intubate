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

function_... <- function(...) ..1

## Functions with any number of parameters. It is suitable for
## functions such as print() or View(), that return NULL.
ntbt_function_... <-
#  ntbt_print <- 
#  ntbt_View <-
  
  function(...) {
    Call <- match.call()
    Call[[1]] <- get_function_name(as.character(Call[[1]]))
    if (is.null(ret <- eval(Call, envir = parent.frame())))
      return (invisible(..1))
    ret
  }


function_data_... <- function(...) ..1

ntbt_function_data_... <-
  ##    I will try some cases and we will see.
  ##    This may be the definitive solution.

#  ntbt_cat <-
  
                      ##           Before
#  ntbt_cor.test <-    ## formula_data, switching

#  ntbt_lm <-          ## formula_data, no switching
#  ntbt_plot <-
#  ntbt_text <-
  
#  ntbt_regsubsets <-  ## x_data
#  ntbt_stripchart <-
  
  ## The Lord of the Rings?
  ## This seems to be the only needed implementation
  ## for *all* functions in pipelines, regardless
  ## of having formula or not.
  ## They always told me, and I also told others,
  ## that attach() / detach() are evil.
  ## Maybe these guys are the ones that will
  ## save the day after all.
  function(data, ...) {
    attach(data)
    Call <- match.call(expand.dots = TRUE)
    Call[[1]] <- get_function_name(as.character(Call[[1]]))
    ret <- eval(Call[-2], envir = parent.frame())
    detach()
    if (is.null(ret))
      return (invisible(data))
    ret
  }

#CO2 %>%
#    ntbt_lm(conc ~ Plant)
  

## *** Alternative implementation ***
##  function(data, formula, ...) {
##    Call <- match.call()
##    fname <- gsub("ntbt_(.+)", "\\1", as.character(Call[[1]]))
##    pars <- as.list(Call)
##
##    pars[2:3] <- pars[3:2]
##    names(pars)[2:3] <- names(pars)[3:2]

##    do.call(fname, pars[-1], envir = parent.frame())
##  }
