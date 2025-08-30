# Random selection
selrand = function(fitvals, ns, ...){
  n=length(fitvals)
  if(missing(ns)) ns=n
  matpool = sample(1:n, size=ns, replace=TRUE)
  return(matpool)
}