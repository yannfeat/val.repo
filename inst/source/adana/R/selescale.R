# exponent scaling
selescale = function(fitvals, ns, selb, ...){
  n = length(fitvals)
  if(missing(ns)) ns=n
  if(missing(selb)) selb=0.5
  fstar = exp((selb*fitvals))
  p = fstar/sum(fstar)
  matpool = sample(1:n, size=ns, prob=p, replace=TRUE)
  return(matpool)
}