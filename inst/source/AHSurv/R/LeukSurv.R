#' The Leukemia Survival Data
#'
#' @name LeukSurv
#' @docType data
#' @author Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Mutua Kilai, \email{abdisalam.hassan@amoud.edu.so}
#' @keywords datasets
#' @description A dataset on the survival of acute myeloid leukemia in 1,043 pateietns, first analyzed by Henderson et al. (2002). It is of interest to investigate possible spatial variation in survival after accounting for known subject-specific prognostic factors, which include age, sex, white blood cell count (wbc) at diagnosis, and the Townsend score (tpi) for which higher values indicates less affluent areas. Both exact residential locations of all patients and their administrative districts (24 districts that make up the whole region) are available.
#' @format A data frame with 1043 rows and 9 variables:
#'\itemize{
#' \item time: survival time in days
#'  \item cens: right censoring status 0=censored, 1=dead
#'  \item xcoord: coordinates in x-axis of residence
#'  \item ycoord: coordinates in y-axis of residence
#'  \item age: age in years
#'  \item sex:male=1 female=0
#'  \item wbc:white blood cell count at diagnosis, truncated at 500
#'  \item tpi: the Townsend score for which higher values indicates less affluent areas
#'  \item district:administrative district of residence
#'}
#'
#'@references Henderson, R., Shimakura, S., and Gorst, D. (2002), Modeling spatial variation in leukemia survival data, \emph{Journal of the American Statistical Association}, 97(460), 965-972.
#'
NULL
