#' Start abstractr
#' @title Launch abstractr GUI
#' @description abstractr() loads interactive user interface built using R shiny.
#' @details The interactive application makes creating visual abstracts fast and intuitive. While the application has certain design elements fixed, namely layout, a vignette is included to guide customizations from scratch.
#' @keywords abstractr
#' @examples
#' \dontrun{
#' library(shiny)
#' abstractr()
#' }
abstractr <- function() {

  runApp(system.file("abstractr", package="abstractr"), launch.browser=TRUE)

}
