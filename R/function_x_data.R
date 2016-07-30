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

function_x_data <- function(x, data, ...) data

## Functions that use *x* followed by *data*.

ntbt_function_x_data <-
  
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
  
  ## Interface for functions that use *x* followed by *data*.
  function(data, x, ...) {
    Call <- match.call()
    Call[[1]] <- as.name(gsub("ntbt_(.+)", "\\1", as.character(Call[[1]])))
    eval(Call, envir = parent.frame())
  }
