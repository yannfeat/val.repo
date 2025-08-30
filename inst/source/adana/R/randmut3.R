# random mutation 3 
randmut3 = function(y, lb, ub, mutpm, ...){
  m = length(y)
  if(missing(lb) | missing(ub))
    stop("randmut3 requires lb and ub!")
  if(missing(mutpm)) mutpm=0.05
  for(j in 1:m){
    r = runif(1, 0, 1)
    val = rnorm(1, mean=0, sd=abs(lb[j]-ub[j]))
    val = ifelse(r < mutpm, val, 0)
    y[j] = y[j] + val
  }
  return(list(mutant=y))
}