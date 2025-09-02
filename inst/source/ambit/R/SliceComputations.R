#####Computing the Lebesgue measure of the slices for a given trawl function
# Input required for simulation on grid 0, Delta, 2 Delta, ...n Delta
# n number of observations
# Delta grid interval
# fct trawl function
# #'Computing the slice sizes
# #'
# #'@title ComputeSliceSizes
# #'@param n number of grid points to be simulated
# #'@param Delta grid-length
# #'@param trawlfct the trawl function used in the simulation
# #'@return Matrix containing all slices
# #'@export

ComputeSliceSizes <- function(n, Delta, trawlfct){

  #Define b_k=\int_0^{\Delta_n}a(k\Delta_n-x)dx, for k=1,..,n+1
  b_vector <- numeric(n+1)
  for(k in 1:(n+1)){
    g_b <- function(x){trawlfct(x-k*Delta)}
    b_vector[k] <- stats::integrate(g_b, 0, Delta)$value
  }

  #Define c_k=b_k-b_{k+1} for k=1,..,n
  c_vector <- numeric(n)
  for(k in 1:n){
    c_vector[k] <- b_vector[k]-b_vector[k+1]
  }

  ###For first column of slice matrix integrate from -infty rather than from 0
  d_vector <- numeric(n+1)
  for(k in 0:n){
    g_d <- function(x){trawlfct(x-k*Delta)}
    d_vector[k+1] <- stats::integrate(g_d, -Inf, 0)$value
  }

  #Define e_k=d_k-d_{k+1} for k=1,..,n
  e_vector <- numeric(n)
  for(k in 1:n){
    e_vector[k] <- d_vector[k]-d_vector[k+1]
  }

  #####Compute matrix of slice sizes
  slice_size_matrix <- matrix(0, n+1, n+1)

  for(k in 1:n){
    slice_size_matrix[k, 1:(n+1-k)] <- rep(c_vector[k], n+1-k)
    slice_size_matrix[k,(n+1-k+1)] <- b_vector[k]
  }
  #Column of first slices:
  slice_size_matrix[,1] <- c(e_vector, d_vector[n+1])

  return(slice_size_matrix)
  #return(list(b_vector, c_vector, d_vector, e_vector, slice_size_matrix))

}


#####
#Adding up the slices for a given (n+1)x(n+1) input matrix
#If the slicematrix contains the Lebesgue measure of the slices, then it
#returns the sequence of trawl sets.
#If the simulated slices are the input, then it returns a trawl path.
#
# #'@title AddSlices
# #'@param slicematrix A matrix of slices.
# #'@return Returns the sum of all slices
# #'@export

AddSlices <- function(slicematrix){
  n <- nrow(slicematrix)-1
  x <- numeric(n+1)
  tmp <- 0
  for(k in 0:n){
    tmp<-0
    for(j in 1:(k+1)){
      tmp<-tmp+sum(slicematrix[(k+2-j):(n+2-j),j])
    }
    x[1+k]<-tmp
  }
  return(x)
}

#####
#Adding up the slices for a given (n+1)x(n+1) input matrix
#weighted by a kernel function
#If the simulated slices are the input, then it returns a path
#of an ambit process.
#
# #'@title AddWeightedSlices
# #'@param slicematrix A matrix of slices
# #'@param weightvector A vector of weights
# #'@return Returns the weighted sum of all slices
# #'@export

AddWeightedSlices <- function(slicematrix, weightvector){
  n <- nrow(slicematrix)-1
  x <- numeric(n+1)
  if(base::length(weightvector)!=(n+1)){
    #print("The weightvector has incorrect length.")
    return(NA)
  }
  else{
  tmp <- 0
  for(k in 0:n){
    tmp<-0
    for(j in 1:(k+1)){
      tmp<-tmp+weightvector[k+2-j]*sum(slicematrix[(k+2-j):(n+2-j),j])
    }
    x[1+k]<-tmp
  }
  return(x)
  }
}




