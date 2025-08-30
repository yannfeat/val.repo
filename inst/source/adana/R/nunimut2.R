# Adaptive Non-uniform mutation
nunimut2 = function(y, lb, ub, g, gmax, mutb, ...){
  m = length(y)
  if(missing(lb) | missing(ub))
    stop("nunimut requires lb and ub !")
  if(missing(g))
    stop("nunimut requires g !")
  if(missing(mutb))
    mutb = 0.5
  if(missing(gmax))
    stop("nunimut requires gmax !")
  v = sample(1:m, 1)
  r = runif(1, 0, 1)
  tau = sample(c(-1,1), 1)
  y[v] = y[v] + tau * (ub[v]-lb[v])*(1-r^(g/gmax)^mutb)
  return(list(mutant=y, mutgen=v))
}