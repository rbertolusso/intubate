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
  if (gsub(paste0("(", prefix, ")_.+"), "\\1", full_name) != prefix)
    stop(paste0(full_name, 
                " is an invalid name.\n", 
                "The interface should be named ntbt_<name>\n",
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
prmt_function_. <-
  prmt_print <- 
  prmt_View <-

  function(...) {
    Call <- match.call()
    Call[[1]] <- get_calling_name("prmt", as.character(Call[[1]]))
    if (is.null(ret <- eval(Call, envir = parent.frame())))
      return (invisible(..1))
    ret
  }



function_._... <- function(., ...) .

ntbt_function_._... <-
  ##    I will try some cases and we will see.
  ##    This may be the definitive solution.

  ntbt_cat <-
  ntbt_print <- 
  ##                            Before
  ntbt_cor.test <-    ## formula_data, switching

  ntbt_lm <-          ## formula_data, no switching
  ntbt_plot <-
  ntbt_text <-
  
  ntbt_regsubsets <-  ## x_data
  ntbt_stripchart <-
  
  ## The Lord of the Rings?
  ## This seems to be the only needed implementation
  ## for *all* functions in pipelines, regardless
  ## of having formula or not. Moreover, it can
  ## be an excellent idea in some cases to start,
  ## instead than from a data.frame or tibble,
  ## from a list (or an environment) so the different
  ## functions can work with variables of different
  ## lengths if necessary.
  ## They always told me, and I also told others,
  ## that attach() / detach() are evil.
  ## Maybe these guys are the ones that will
  ## save the day after all.
  function(., ...) {
    attach(.)
    Call <- match.call()
    Call[[1]] <- get_calling_name("ntbt", as.character(Call[[1]]))
    ret <- eval(Call[-2], envir = parent.frame())
    detach()
    if (is.null(ret))
      return (invisible(.))
    ret
  }

#library(magrittr)
#CO2 %>%
#    ntbt_lm(conc ~ Type)
