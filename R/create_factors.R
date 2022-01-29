# bcrtools: a little R-function collection for ecological statistics
# Copyright (C) 2022  Bjoern C. Rall
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' Transform all character strings to factors or update meta-data from existing
#' factors
#'
#' @param ds Your data-set as data.frame.
#'
#' @return A data frame with factors if the input variables where either the
#'     class "character" or "factor".
#'
#' @description This function converts ALL character-strings variables in a
#'     data frame into factorial variables. In addition it updates the meta-
#'     data from already existing factorial variables.
#'
#' @export
#'
#' @examples
#'
#' # no example yet
#'

create_factors <- function(ds){

  for(i in 1:ncol(ds)){
    if(class(ds[,i]) == "character"){
      ds[,i] <- as.factor(ds[,i])
    }

    if(class(ds[,i]) == "factor"){
      ds[,i] <- as.factor( as.character(ds[,i]) )
    }
  }

  return(ds)
}
