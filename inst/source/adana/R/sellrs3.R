# Linear ranking selection 3
sellrs3 = function(fitvals, ns, sels, ...){
  n = length(fitvals)
  if(missing(ns)) ns=n
  if(missing(sels)) sels=1.5 
  if(sels<1 | sels>1.99) sels=1.5
  fitranks = rank(fitvals, ties.method="min")
  p = 1/n*(sels-2*(sels-1)*((fitranks-1)/(n-1)))
  matpool = sample(1:n, size=ns, prob=p)
  return(matpool)
}