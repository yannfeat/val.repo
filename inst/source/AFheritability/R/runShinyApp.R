#' @title The shiny application \code{AFheritability} is a user interface for the function \code{\link[AFheritability]{AFfunction}}
#' @description The shiny-app provides a user friendly interface for the function \code{\link[AFheritability]{AFfunction}}.
#' @author Elisabeth Dahlqwist
#' @details By running runShinyApp() a user interface for the function \code{\link[AFheritability]{AFfunction}} is started in RStudio. The app is also available online \url{https://afheritability.shinyapps.io/afheritability/} (Note that the app is usually faster in the web browser Google Chrome or Firefox).
#' @references Dahlqwist E et al. (2019) <doi:10.1007/s00439-019-02006-8>.
#' @import shiny
#' @export
runShinyApp <- function() {
  appDir <- system.file("shiny-examples", "AFheritability_shiny", package = "AFheritability")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `AFheritability`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
