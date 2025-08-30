# Initialization function
initialize = function(initfunc, n, m, type, ...){
  func = as.character(match.call()[2])
  if(missing(type)) type=1
  dotargs = list(...)
  dotargs$n = n
  dotargs$m = m
  dotargs$type = type
  initpop=do.call(func, dotargs)
  return(initpop)
}