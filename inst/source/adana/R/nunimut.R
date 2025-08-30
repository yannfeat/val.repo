# non-uniform mutation
nunimut = function(y, lb, ub, g, gmax, mutb, ...){
  m = length(y)
  if(missing(lb) | missing(ub))
    stop("nunimut requires lb and ub !")
  if(missing(g))
    stop("nunimut requires q !")
  if(missing(mutb))
    mutb = 0.5
  if(missing(gmax))
    stop("nunimut requires gmax !")
  v = sample(1:m, 1)
  r = runif(1, 0, 1)
  if(r<=0.5)
    y[v] = y[v]+(ub[v]-y[v]) * r * (1-g/gmax)^mutb
  else
    y[v] = y[v]-(y[v]-lb[v]) * r * (1-g/gmax)^mutb
  return(list(mutant=y, mutgen=v))
}
