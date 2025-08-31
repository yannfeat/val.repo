#' NDVI
#'
#' This function calculate NDVI value from input near-infrared (NIR) and red bands images.
#' If change the input from red band into green or red-edge band values, it will return GNDVI or NDRE values.
#'
#' @usage NDVI(NIR, R)
#' @param NIR a dataframe or matrix contains NIR band values.
#' @param R a dataframe or matrix contains red band values, NIR and R should have same class and dimension..
#' @return A matrix or dataframe(depends on the class of NIR and R).
#' @export
#'
#' @references
#' De Swaef, T., Maes, W. H., Aper, J., Baert, J., Cougnon, M., Reheul, D., ... & Lootens, P. (2021). Applying RGB-and thermal-based vegetation indices from UAVs for high-throughput field phenotyping of drought tolerance in forage grasses. Remote Sensing, 13(1), 147.

NDVI <- function(NIR,R){

  if (all.equal(dim(NIR), dim(R)) != TRUE) {
    stop('NIR and R should have the same dimensions')
  }

  if(anyNA(NIR)|anyNA(R) == TRUE){
    stop('NIR and R contain NAs')
  }

  if (any(NIR < 0, na.rm=  TRUE)) {
    stop("NIR contains negative values. All values must be greater than 0.")
  }
  if (any(R < 0, na.rm=  TRUE)) {
    stop("R contains negative values. All values must be greater than 0.")
  }


  if (any(R > 255, na.rm=  TRUE)) {
    stop("All values of R must be less than 255.")
  }

  if(class(NIR) %in% c('data.frame','matrix') == F){
    stop(paste0("Object of NIR is ", class(NIR), ". class(NIR) should be 'data.frame' or 'matrix'"))
  }
  if(class(R) %in% c('data.frame','matrix') == F){
    stop(paste0("Object of R is ", class(R), ". class(R) should be 'data.frame' or 'matrix'"))
  }

  if(length(unique(c(class(NIR),class(R)))) > 1){
    stop('Object of NIR and R should have the same class')
  }

  ndvi <- (NIR - R)/(NIR + R)
  return(ndvi)
}
