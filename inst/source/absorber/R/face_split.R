face_split <- function(x,y){
  if (!(is.matrix(x)&&is.matrix(y))) stop("Arguments must be matrices.")
  if (dim(x)[1]!=dim(y)[1]) stop("Arguments must have same number of rows")
  retmat <- matrix(0,nrow=dim(x)[1], ncol=dim(x)[2]*dim(y)[2])
  for (j in 1:nrow(retmat)) retmat[j,] <- kronecker(x[j,],y[j,])
  retmat
}