# Insertion + Inversion mutation
insswapmut = function(y, ...){
  n = length(y)
  v = sample(1:n, 1)
  k = sample(1:(n-1), 1)
  idx = c(setdiff(1:k,v), v, setdiff((k+1):n,v))
  y = y[idx]
  v = sort(sample(1:n, size=2, replace=FALSE))
  subgenes = y[v[1]:v[2]] 
  subgenes = rev(subgenes)
  y[v[1]:v[2]] = subgenes
  return(list(mutant=y, mutgen=v))
}