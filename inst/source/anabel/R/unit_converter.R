# Copyright (C) 2025  Stefan Kraemer
#   
#   This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation as version 3 of the License
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.



#' Convert a unit to molar
#'
#' @description convert the value into molar.
#' @param val numeric value of the analyte concentration
#' @param unit character string indicating the unit from which, the analyte concentration will be converted into molar.
#'
#' @details supported units are: millimolar, micromolar, nanomolar and picomolar. The name of the unit could be written, or its
#' abbreviation such as: nanomolar (nm), micromolar (mim), picomolar (pm), or millimolar (mm). The unite in either form is case insensitive.
#'
#' @return The value of analyte concentration in molar
#'
#' @examples convert_toMolar(120, "nanomolar")
#' convert_toMolar(120, "nm")
#' convert_toMolar(120, "millimolar")
#' convert_toMolar(120, "mm")
#' convert_toMolar(120, "micromolar")
#' convert_toMolar(120, "mim")
#' convert_toMolar(120, "picomolar")
#' convert_toMolar(120, "pm")
#'
#' @export

convert_toMolar <- function(val, unit) {
  switch(tolower(unit),
    millimolar = as.numeric(val) / 1000,
    mm = as.numeric(val) / 1000,
    micromolar = as.numeric(val) / 1e+6,
    mim = as.numeric(val) / 1e+6,
    nanomolar = as.numeric(val) / 1e+9,
    nm = as.numeric(val) / 1e+9,
    picomolar = as.numeric(val) / 1e+12,
    pm = as.numeric(val) / 1e+12,
    "Invalid input! accepted units are: Millimolar, Micromolar, Nanomolar, or Picomolar!"
  )
}
