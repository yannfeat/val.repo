# Shuffle Mutation
shufmut = function(y, ...){
  n = length(y)
  v = sort(sample(1:n, size=2, replace=FALSE))
  subgenes = y[v[1]:v[2]] 
  subgenes = sample(subgenes)
  y[v[1]:v[2]] = subgenes
  return(list(mutant=y, mutrange=v))
}