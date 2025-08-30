#'Insert Equal
#'
#'Add = operator in line of code in RStudio
#'
#'@usage insert_equal()
#'@export
#'
#'@examples
#'\dontrun{insert_equal()}
insert_equal <- function() {
  rstudioapi::insertText(text = " = ", id = NULL)
}
