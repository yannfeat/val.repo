# random mutation 4 
randmut4 = function(y, lb, ub,...){
  m = length(y)
  if(missing(lb) | missing(ub))
    stop("randmut4 requires lb and ub!")
  v = sample(1:m, 1)
  r = runif(1, -0.1, 0.1)
  y[v] = y[v] + r * (ub[v]-lb[v])
  return(list(mutant=y, mutgen=v))
}