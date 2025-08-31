#' Africa Macroeconomic Monitor Database API
#'
#' An R API providing access to a relational database with macroeconomic data for Africa. The
#' database is maintained at the Kiel Institute for the World Economy.
#'
#' @section Functions:
#' Functions and data providing information about the available data
#'
#' \code{\link[=am_sources]{am_sources()}}\cr
#' \code{\link[=am_series]{am_series()}}\cr
#' \code{\link{am_countries}}\cr
#' \code{\link{am_countries_wld}}\cr
#' \code{\link{am_entities}}
#'
#' Function to retrieve the data from the database
#'
#' \code{\link[=am_data]{am_data()}}
#'
#' Functions to reshape data and add temporal identifiers
#'
#' \code{\link[=am_pivot_wider]{am_pivot_wider()}}\cr
#' \code{\link[=am_pivot_longer]{am_pivot_longer()}}\cr
#' \code{\link[=am_expand_date]{am_expand_date()}}
#'
#' % Function to export wide format data to Excel
#' %
#' % \code{\link[=am_write_excel]{am_write_excel()}}\cr
#' %
#' Helper functions to convert inputs to R dates
#'
#' \code{\link[=am_as_date]{am_as_date()}}\cr
#' % \code{\link[=am_transpose]{am_transpose()}}\cr
#'
#' Global Macros with core ID variables in the database
#'
#' \code{\link{.AMID}}\cr
#' \code{\link{.AMT}}
#'
#'
#' @docType package
#' @name africamonitor-package
#' @aliases africamonitor
#'
#' @importFrom utils packageVersion assignInMyNamespace
#' @importFrom stats as.formula setNames
#' @importFrom DBI dbConnect dbGetQuery dbDisconnect
#' @importFrom RMySQL MySQL
#' @importFrom collapse pivot collapv ffirst fmedian funique ftransformv get_vars get_vars<- date_vars add_vars add_vars<- cat_vars ss qF vlabels vlabels<- ckmatch qDT fnobs fnrow fncol unattrib namlab allNA whichNA
#' @importFrom data.table setDT fifelse melt transpose setcolorder
# #' @importFrom writexl write_xlsx
#'
NULL


.onAttach <- function(libname, pkgname) {

  packageStartupMessage(paste0("africamonitor ", packageVersion("africamonitor"), ", see help(africamonitor)"))

}

.onUnload <- function(libpath) {

  if(length(.am_con)) tryCatch(dbDisconnect(.am_con), error = function(e) cat(""))

}

.connect <- function() {
  tryCatch({

    if(isTRUE(getOption("africamonitor_localhost"))) {
      dbConnect(MySQL(), user = 'IFW_READ_LOCAL', password = '$QL5Dbg8+^g`)$D.',
                dbname = 'AFRMDB', host = 'localhost')
    } else {
      dbConnect(MySQL(), user = Sys.getenv("AFRM_DB_READ_USER", "AFRMREAD"),
                password = Sys.getenv("AFRM_DB_READ_PASSWORD", "JGW0vyw1efe.utq!acd"),
                dbname = Sys.getenv("AFRM_DB_NAME", "AFRMDB"),
                port = as.integer(Sys.getenv("AFRM_DB_READ_PORT", "4008")),
                host = Sys.getenv("AFRM_DB_HOST", "africamonitor-api.ifw-kiel.de"))
    }

  }, error = function(e) {
    message("Could not connect to database. Please make sure your internet connection is working, and your firewall does not block remote IP connections.")
    NULL})
}





