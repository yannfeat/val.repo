# Gauss mutation 3
gaussmut3 = function(y, mutmy, mutsdy, ...){
  m = length(y)
  if(missing(mutmy) | missing(mutsdy)) return(0)
  val=NA
  while(is.na(val)){
    v = sample(1:m, 1)
    val = rnorm(1, mean=mutmy[v], sd=mutsdy[v])
  }
  y[v] = val
  return(list(mutant=y, mutgen=v))
}
