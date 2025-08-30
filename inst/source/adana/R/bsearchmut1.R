# Mutation specific to search in bounds 1
bsearchmut1 = function(y, mutq, ...){
  m = length(y)
  if(missing(mutq)) mutq=runif(1)
  v = sample(1:m, 2, replace=FALSE)
  y[v[1]] = mutq*y[v[1]]
  y[v[2]] = mutq/y[v[2]]
  return(list(mutant=y, mutgen=v))
}
