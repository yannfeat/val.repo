# Insert mutation
insmut = function(y, ...){
  n = length(y)
  v = sample(1:n, 1)
  k = sample(1:(n-1), 1)
  idx = c(setdiff(1:k,v), v, setdiff((k+1):n,v))
  y <- y[idx]
  return(list(mutant=y, mutgen=v, mutpoint=k))
}