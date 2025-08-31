#' RGB based Vegetative Indexes (RGBVI)
#'
#' This function calculate some useful RGB based vegetative indexes.
#'
#' The \code{vi} parameter should be a characteristic vector and include
#' at least one of the following VI: 'RCC','GCC','ExG2','ExR','ExGR','GRVI','VDVI','VARI','MGRVI','CIVE','VEG'.
#' By default, all of the VIs will be calculated.
#'
#' @usage RGBVI(R, G, B, vi = c('RCC','GCC','ExG2','ExR','ExGR'
#' ,'GRVI','VDVI','VARI','MGRVI','CIVE','VEG'))
#' @param R a dataframe or matrix contains 'red' values, RGB should have same class and dimension..
#' @param G a dataframe or matrix contains 'green' values, RGB should have same class and dimension.
#' @param B a dataframe or matrix contains 'blue' values, RGB should have same class and dimension..
#' @param vi vegetative indexes to be calculated(see Details).
#' @return A list with length(vi). Each elements represent a vegetative index matrix or data frame.
#' @export
#' @examples
#' # generate R, G, B matrix with range 0~255
#' R <- matrix(sample(0:255,25), nrow=5, ncol=5)
#' G <- matrix(sample(0:255,25), nrow=5, ncol=5)
#' B <- matrix(sample(0:255,25), nrow=5, ncol=5)
#'
#' # calculate all available rgb vi
#' vi.all <- RGBVI(R,G,B)
#'
#' # calculate RCC,GCC,ExGR,MGRVI
#' vi.4 <- RGBVI(R,G,B,vi=c('RCC','GCC','ExGR','MGRVI'))
#'
#' @references
#' De Swaef, T., Maes, W. H., Aper, J., Baert, J., Cougnon, M., Reheul, D., ... & Lootens, P. (2021). Applying RGB-and thermal-based vegetation indices from UAVs for high-throughput field phenotyping of drought tolerance in forage grasses. Remote Sensing, 13(1), 147.


RGBVI <- function(R, G, B, vi = c('RCC','GCC','ExG2','ExR','ExGR','GRVI','VDVI','VARI','MGRVI','CIVE','VEG')){

  if (all.equal(dim(R), dim(G)) != TRUE) {
    stop('R, G, B should have the same dimensions')
  }
  if (all.equal(dim(R), dim(B)) != TRUE) {
    stop('R, G, B should have the same dimensions')
  }
  if (all.equal(dim(B), dim(G)) != TRUE) {
    stop('R, G, B should have the same dimensions')
  }

  if(anyNA(R)|anyNA(G)|anyNA(B) == TRUE){
    stop('R, G, B contain NAs')
  }

  if (any(R < 0, na.rm=  TRUE)) {
    stop("R contains negative values. All values must be greater than 0.")
  }
  if (any(G < 0, na.rm = TRUE)) {
    stop("G contains negative values. All values must be greater than 0.")
  }
  if (any(B < 0, na.rm = TRUE)) {
    stop("B contains negative values. All values must be greater than 0.")
  }

  if (any(R > 255, na.rm=  TRUE)) {
    stop("All values of R must be less than 255.")
  }
  if (any(G > 255, na.rm = TRUE)) {
    stop("All values of G must be less than 255.")
  }
  if (any(B > 255, na.rm = TRUE)) {
    stop("All values of B must be less than 255.")
  }

  if(is.data.frame(R)){
    R <- as.matrix(R)
  }
  if(is.data.frame(G)){
    G <- as.matrix(G)
  }
  if(is.data.frame(B)){
    B <- as.matrix(B)
  }

  if(!is.matrix(R)){
    stop(paste0("Object of R is ", class(R), ". class(R) should be 'data.frame' or 'matrix'"))
  }
  if(!is.matrix(G)){
    stop(paste0("Object of G is ", class(G), ". class(G) should be 'data.frame' or 'matrix'"))
  }
  if(!is.matrix(B)){
    stop(paste0("Object of G is ", class(G), ". class(G) should be 'data.frame' or 'matrix'"))
  }

  if (!inherits(vi, 'character')) {
    stop('vi must be a character vector')
  }
  avail_vi <- c('RCC','GCC','ExG2','ExR','ExGR','GRVI','VDVI','VARI','MGRVI','CIVE','VEG')
  vi_check <- unlist(lapply(vi, function(vi) vi %in% avail_vi))
  if (sum(vi_check) != length(vi_check)) {
    stop(paste('invalid vi(s):',
               paste(vi[!vi_check], collapse=', ')))
  }

  output <- vector('list',length(vi))
  names(output) <- vi

  if('RCC' %in% vi){
    output$RCC <- R/R+G+B
  }
  if ('GCC' %in% vi){
    output$GCC <- G/R+G+B
  }
  if ('ExG2' %in% vi){
    output$ExG2 <- (2*G-B-R)/(R+G+B)
  }
  if ('ExR' %in% vi){
    output$ExR <- (1.4*R-G)/(R+G+B)
  }
  if ('ExGR' %in% vi){
    exg2 <- (2*G-B-R)/(R+G+B)
    exr <- (1.4*R-G)/(R+G+B)
    output$ExGR <- exg2-exr
  }
  if ('GRVI' %in% vi){
    output$GRVI <- (G-R)/(G+R)
  }
  if ('VDVI' %in% vi){
    output$VDVI <- (2*G-R-B)/(2*G+R+B)
  }
  if ('VARI' %in% vi){
    output$VARI <- (G-R)/(G+R-B)
  }
  if ('MGRVI' %in% vi){
    output$MGRVI <- (G^2-R^2)/(G^2+R^2)
  }
  if ('CIVE' %in% vi){
    output$CIVE <- 0.441*R - 0.881*G + 0.385*B + 18.787
  }
  if ('VEG' %in% vi){
    output$VEG <- G/(R^0.667*B^0.334)
  }

  return(output)
}
