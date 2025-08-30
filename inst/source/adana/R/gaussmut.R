# Gauss mutation
gaussmut = function(y, mutsdy, ...){
  m = length(y)
  if(missing(mutsdy)) mutsdy=1
  v = sample(1:m, 1)
  rval = rnorm(1, 0, mutsdy[v])
  y[v] = y[v]+rval
  return(list(mutant=y, mutgen=v))
}
