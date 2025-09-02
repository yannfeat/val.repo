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

#' @importFrom utils packageVersion
#'

.onAttach <- function(libname, pkgname) {
  msg <- paste0("Thank you for using anabel (", packageVersion("anabel"), "), please cite anabel by using: citation(package = 'anabel').")
  packageStartupMessage(msg)
}

.onLoad <- function(libname, pkgname) {
  check_dependencies()
}
