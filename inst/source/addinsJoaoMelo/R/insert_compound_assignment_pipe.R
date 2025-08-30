#'Insert Compound Assignment pipe
#'
#'Add \%<>\% pipe in line of code in RStudio
#'
#'@usage insert_compound_assignment_pipe()
#'@examples
#'\dontrun{insert_compound_assignment_pipe()}
#'@export
insert_compound_assignment_pipe<- function() {
  rstudioapi::insertText(text = " %<>% ", id = NULL)
}
