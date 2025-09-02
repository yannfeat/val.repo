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


#' @importFrom kableExtra kable_styling
#' @importFrom kableExtra scroll_box

check_dependencies <- function(libs = NULL) {
  if (is.null(libs)) {
    libs <- c(
      "cli", "dplyr", "ggplot2", "minpack.lm",
      "openxlsx", "progress", "purrr", "qpdf",
      "reshape2", "rlang", "stats", "utils",
      "tidyr"
    )
  } # required only when output is generated

  for (pkg in libs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is missing; you need to manually install it."))
    }
  }
}
