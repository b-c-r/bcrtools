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

#' A wrapper around the read.csv function allowing for creating factors on the fly
#'
#' @param pathname The path to your csv file.
#' @param sep The column separator, default is `,`.
#' @param dec the decimal separator, default is `.`.
#' @param create_factors If `TRUE`, all columns will be returned as factorial, default is `TRUE`.
#' @param show_structure Shows the structure of the final data via `str()`.
#' @param show_head Shows the head of the final data via `head()`.
#'
#' @return A data frame with factors if the input data was non-numbers (integers or floats).
#'
#' @export
#'
#' @examples
#'
#' # no example yet
#'


read_data <- function(pathname,
                      sep = ",",
                      dec = ".",
                      create_factors = T,
                      show_structure = F){

  ds <- read.csv(pathname, sep = sep, dec = dec)

  if(create_factors == TRUE){
    ds <- create_factors(ds)
  }

  if(show_structure == TRUE) str(ds)

  return(ds)
}
