# Power mutation 2
powmut2 = function(y, lb, ub, mutpow, ...){
  m = length(y)
  if(missing(lb) | missing(ub)) 
    stop("powmut requires lb and ub !")
  if(missing(mutpow)) mutpow = 2
  if(length(mutpow) < m) mutpow = rep(mutpow, m)
  v = sample(1:m, 1)
  r = runif(1)
  p = runif(1)^mutpow[v]
  t = (y[v]-lb[v])/(ub[v]-y[v])
  y[v] = y[v] + ifelse(r>t, -p*(y[v]-lb[v]), +p*(ub[v]-y[v]))
  return(list(mutant=y, mutgen=v))
}