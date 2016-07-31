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

get_function_name <- function(ntbt_name) {
  if (gsub("(ntbt_).+", "\\1", ntbt_name) != "ntbt_")
    stop(paste0(ntbt_name, 
                " is an invalid name.\n", 
                "The interface should be named xtbt_<name>\n",
                "where <name> is the name of the function to be interfaced."))
  as.name(gsub("ntbt_(.+)", "\\1", ntbt_name))
}
