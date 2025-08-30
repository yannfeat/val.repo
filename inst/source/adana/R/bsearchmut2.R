# Mutation specific to search in bounds 2
bsearchmut2 = function(y, ...){
  m = length(y)
  v = sample(1:m, 2, replace=FALSE)
  p = runif(1)
  q = sqrt((y[v[1]]/ y[v[2]])^2 * (1-p^2) + 1)
  y[v[1]] = p*y[v[1]]
  y[v[2]] = q*y[v[2]]
  return(list(mutant=y, mutgen=v))
}
