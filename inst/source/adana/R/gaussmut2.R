# Gauss mutation 2
gaussmut2 = function(y, ...){
  m = length(y)
  v = sample(1:m, 1)
  y[v] = y[v]+rnorm(1)
  return(list(mutant=y, mutgen=v))
}
