# Sigma scaling
selsscale = function(fitvals, ns, selc, ...){
  n = length(fitvals)
  if(missing(ns)) ns=n
  if(missing(selc)) selc=2
  favg = mean(fitvals)
  fsd = sd(fitvals)
  fstar = rep(NA, n)
  for(i in 1:n)
    fstar[i] = ifelse(fsd!=0, 1+(fitvals[i]-favg)/selc*fsd, 1.0)
  fstar[fstar<0]=0
  p = fstar/sum(fstar)
  matpool = sample(1:n, size=ns, prob=p, replace=TRUE)
  return(matpool)
}