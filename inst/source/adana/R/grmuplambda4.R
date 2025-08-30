# Mu+Lambda replacement function 4 
grmuplambda4 = function(parpop, offpop, lambda, ...){
  if(missing(lambda)) lambda = round(nrow(offpop)/2)
  npar = nrow(parpop)
  noff = nrow(offpop)
  paridx = sample(1:npar, size=lambda, replace=FALSE)
  offidx = sample(1:noff, size=lambda, replace=FALSE)
  parpop[paridx,] = offpop[offidx,]
  return(parpop)
}
