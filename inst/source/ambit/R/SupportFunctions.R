#'Returns summary statistics
#'@title my_results
#'@param x data
#'@param sd Optional parameter giving the standard deviation of the normal
#'distribution used for computing the coverage probabilities
#'@param digits Optional parameter to how many digits the results
#'should be rounded, the default is three.
#'@details This functions returns the sample mean, sample standard deviation
#'and the coverage probabilities at level 75\%, 80\%, 85\%, 90\%, 95\%, 99\%
#'compared to the standard normal quantiles.
#'@return The vector of the sample mean, sample standard deviation
#'and the coverage probabilities at level 75\%, 80\%, 85\%, 90\%, 95\%, 99\%
#'compared to the standard normal quantiles.
#'@export
#'@example man/examples/my_results.R
my_results <-function(x, sd=1, digits=3){
  n <- base::length(x)
  value <- c(
    base::mean(x),
    sqrt(stats::var(x)),
    #75%
    sum(abs(x)<stats::qnorm(0.875, mean=0, sd = sd))/n,
    #80%
    sum(abs(x)<stats::qnorm(0.9, mean=0, sd = sd))/n,
    #85%
    sum(abs(x)<stats::qnorm(0.925, mean=0, sd = sd))/n,
    #90%
    sum(abs(x)<stats::qnorm(0.95, mean=0, sd = sd))/n,
    #95%
    sum(abs(x)<stats::qnorm(0.975, mean=0, sd = sd))/n,
    #99%
    sum(abs(x)<stats::qnorm(0.995, mean=0, sd = sd))/n
  )
  return(base::round(value, digits))
}



#'Returns the mean squared error between two vectors
#'@title my_mse
#'@param x vector
#'@param y vector
#'@return Mean square error between the two vectors x and y
#'@example man/examples/my_mse.R
#'@export
my_mse <-function(x, y){
  length<-base::length(x)
  return(sum((x-y)^2)/length)
}

#'Returns the mean absolute error between two vectors
#'@title my_mse
#'@param x vector
#'@param y vector
#'@return Mean absolute error between the two vectors x and y
#'@export
#'@example man/examples/my_mae.R
my_mae <-function(x, y){
  length<-base::length(x)
  return(sum(abs(x-y))/length)
}
