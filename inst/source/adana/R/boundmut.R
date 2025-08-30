# Boundary mutation
boundmut = function(y, lb, ub, ...){
  if(missing(lb) | missing(ub)) 
    stop("boundmut requires lb and ub!")
  v = sample(1:length(y), 1)
  r = runif(1, 0,1)
  y[v] = ifelse(r>0.5, ub[v], lb[v])
  return(list(mutant=y, mutgen=v))
}