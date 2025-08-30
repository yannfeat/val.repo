# random replacement mutation
randmut = function(y, lb, ub, ...){
  m = length(y)
  if(missing(lb) | missing(ub)) 
    stop("randmut requires lb and ub!")
  j = sample(1:m, 1)
  y[j] = runif(1, lb[j], ub[j])
  return(list(mutant=y, mutgen=j))
}
