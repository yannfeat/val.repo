# Inverse mutation
invmut = function(y, ...){
  n = length(y)
  v = sort(sample(1:n, size=2, replace=FALSE))
  subgenes = y[v[1]:v[2]] 
  subgenes = rev(subgenes)
  y[v[1]:v[2]] = subgenes
  return(list(mutant=y, mutrange=v))
}
