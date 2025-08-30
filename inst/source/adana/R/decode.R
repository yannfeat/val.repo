# Convert from binary number to real number
decode = function(bin, lb, ub, m){
  if(missing(lb) | missing(ub)) stop("lb, ub eksik")
  if(missing(m)) m=length(bin)
  real = lb + (ub-lb)/(2^m-1) * sum(c(bin)*c(2^seq(m-1,0)))
  return(real)
}