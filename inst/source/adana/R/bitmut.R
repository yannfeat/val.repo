# Bit conversion mutation
bitmut = function(y, ...){
  m = length(y)
  v = sample(1:m, 1)
  y[v] = ifelse(y[v], 0, 1)
  return(list(mutant=y, mutgen=v))
}