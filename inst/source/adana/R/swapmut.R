# Swap mutation
swapmut = function(y, ...){
  n = length(y)
  v = sort(sample(1:n, size=2, replace=FALSE))
  takas = y[v[1]] 
  y[v[1]] = y[v[2]]
  y[v[2]] = takas
  return(list(mutant=y, mutgen=v))
}
