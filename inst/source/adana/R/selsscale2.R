# Sigma scaling 2
selsscale2 = function(fitvals, ns, selc, ...){
  n = length(fitvals)
  if(missing(ns)) ns=n
  if(missing(selc)) selc=2
  if(any(fitvals<0)) fitvals=fitvals-min(fitvals)
  favg = mean(fitvals)
  fsd = sd(fitvals)
  fstar = fitvals + (favg-selc*fsd)
  fstar[fstar<0]=0
  p = fstar/sum(fstar)
  matpool = sample(1:n, size=ns, prob=p, replace=TRUE)
  return(matpool)
}
