# Mu+Lambda replacement function 3 
grmuplambda3 = function(parpop, offpop, lambda, ...){
  if(missing(lambda)) lambda = round(nrow(offpop)/2)
  n = nrow(offpop)
  paridx = sample(1:n, size=lambda, replace=FALSE)
  offpop = offpop[order(offpop[, "fitval"], decreasing=TRUE),]
  parpop[paridx,] = offpop[1:lambda,]
  return(parpop)
}
