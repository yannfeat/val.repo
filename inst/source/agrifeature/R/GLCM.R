#' Gray Level Co-occurrence Matrix(GLCM).
#'
#' This function calculate Gray Level Co-occurrence Matrix(GLCM), which can be used to describe texture of the image.
#' The default parameters is distance = 1, angle = 0, gray level = 8
#'
#' @usage GLCM(x, d = 1, angle = 0, ngray = 8, grayscale = c(0,255), symmetric = TRUE, prob = FALSE)
#' @param x A numeric matrix.
#' @param d an integer value, the distance between the current pixel, and the
#'   pixel to which it is compared.
#' @param angle one of "0", "45", "90" or "135", the pixel to which the
#'   current pixel is compared.
#' @param ngray an integer value, the number of gray levels to use in calculate GLCM.
#' @param grayscale a vector which contain c(min,max) to set the range of value
#' , if NULL grayscale will be set into the min and max value in x.
#' @param symmetric Logical value, if TRUE (default) the matrix will be turn into a symmetric GLCM.
#' @param prob Logical value, if TRUE (default) the matrix will be normalized such that
#'   the sum of it's components is 1.
#' @export
#' @return A GLCM with dimension ngray*ngray table. Each column and row represent a gray level in the image matrix.
#' @examples
#' # generate an image data matrix with range 0~255
#' set.seed(100)
#' m <- matrix(sample(0:255,64), nrow=8, ncol=8)
#'
#' # calculate GLCM with defalut parameters
#' GLCM.m <- GLCM(m)
#'
#' # calculate probability GLCM
#' GLCM.m.p <- GLCM(m,prob = TRUE)
#'
#' @references
#' Hall-Beyer, M. (2000). GLCM texture: a tutorial. National Council on Geographic Information and Analysis Remote Sensing Core Curriculum, 3, 75.

GLCM <- function(x, d=1, angle = 0, ngray = 8, grayscale = c(0,255), symmetric = TRUE, prob = FALSE){

  if (!is.matrix(x)) {
    stop(paste0("Object of x is ", class(x), ".  is.matrix(x) must evaluate TRUE."))
  }

  if (any(x < 0, na.rm=TRUE)) {
    stop("Object contains negative values. All values must be greater than 0.")
  }

  if(length(unique(c(x))) == 0){
    stop("Function not valid for empty input")
  }

  if(sum(is.na(x)) == length(x)){
    stop("Matrix composed entirely of NA's")
  }

  if (!is.numeric(ngray)) {
    stop(paste0("Object of ngray is ", class(ngray), ".  is.numeric(ngray) must evaluate TRUE."))
  }

  if (!is.numeric(d)) {
    stop(paste0("Object of d is ", class(d), ".  is.numeric(d) must evaluate TRUE."))
  }

  if(is.null(grayscale)){
    grayscale <- c(min(x),max(x))
  }
  x <- apply(x,2,findInterval,seq(grayscale[1],grayscale[2],grayscale[2]/ngray))

  if(identical(angle, 0)){
    theta1 <- 0;theta2 <- 1
  } else if (identical(angle, 45)){
    theta1 <- -1;theta2 <- 1
  } else if (identical(angle, 90)){
    theta1 <- -1;theta2 <- 0
  } else if (identical(angle, 135)){
    theta1 <- -1;theta2 <- -1
  } else {
    stop("angle must be one of '0', '45', '90', '135'.")
  }

  #calculate GLCM matrix
  ri = match((1:nrow(x))-d*theta1,1:nrow(x))
  ci = match((1:ncol(x))+d*theta2,1:ncol(x))
  x.new = x[ri, ci]

  tf <- as.data.frame(table(c(x),c(x.new)))
  df <- as.matrix(table(c(x),c(x.new)))

  if(is.null(ngray)){
    ngray <- grayscale[2]
  }

  if(sqrt(nrow(tf)) != ngray){
    m <- matrix(0,nrow = ngray, ncol = ngray)
    colnames(m) <- rownames(m) <- 1:ngray

    for(i in 1:ngray){
      for(j in 1:ngray){
        if(length(tf[which(tf[,1] == i & tf[,2] == j) ,3]) != 0){
          m[i,j] <- tf[which(tf[,1] == i & tf[,2] == j) ,3]
        }
      }
    }
    df <- m
  }

  #turn GLCM into symmetric matrix
  if(symmetric){
    df <- df + t(df)
  }

  if(prob){
    sdf <- sum(df)
    if(sdf > 0){
      df <- df/sdf
    }
  }
  return(df)
}



