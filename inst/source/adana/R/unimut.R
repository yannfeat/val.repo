# Uniform value mutation
unimut = function(y, lb, ub, ...){
  if(missing(lb) | missing(ub)) 
    stop("unimut requires lb and ub!")
  v = sample(1:length(y), 1)
  y[v] = runif(1, lb[v], ub[v])
  return(list(mutant=y, mutgen=v))
}