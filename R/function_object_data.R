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

function_object_data <- function(object, data, ...) data

## Functions that use *object* followed by *data*
ntbt_function_object_data <-
  
  ## lattice
  ntbt_tmd <-
  
  ## MASS
  ntbt_logtrans <-
  
  ## nlme <-
  ntbt_lmList <-

  ## stats
  ntbt_alias <-
  ntbt_getInitial <-
  ntbt_model.matrix <-

  ## Interface for functions that use *object* followed by *data*
  function(data, object, ...) {
    Call <- match.call()
    Call[[1]] <- get_function_name(as.character(Call[[1]]))
    eval(Call, envir = parent.frame())
  }
