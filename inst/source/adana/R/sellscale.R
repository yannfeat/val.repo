# Linear fitness scaling
sellscale = function(fitvals, ns, sells, ...){
  n=length(fitvals)
  if(missing(ns)) ns=n
  if(missing(sells)) sells=1.5 # Scaling factor
  fmin = min(fitvals)
  fmax = max(fitvals)
  favg = mean(fitvals)
  fstar = rep(NA, n)
  if(sells > (1+(fmax-favg)/(favg-fmin)))
    ms = (fmax-favg)/(favg-fmin)
  else
    ms = sells-1.0
  fstar = 1+ms*(fitvals-favg)/(fmax-favg) # Scaled fitness
  p=fstar/sum(fstar)
  matpool=sample(1:n, size=ns, prob=p, replace=TRUE)
  return(matpool)
}