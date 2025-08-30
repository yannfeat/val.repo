# Mu+Lambda replacement function 2 (delete the worst lambda)
grmuplambda2 = function(parpop, offpop, lambda, ...){
  if(missing(lambda)) lambda = round(nrow(offpop)/2)
  n = nrow(offpop)
  parpop = parpop[order(parpop[, "fitval"], decreasing=TRUE),]
  offpop = offpop[order(offpop[, "fitval"], decreasing=TRUE),]
  parpop[(n-lambda+1):n,] = offpop[1:lambda,]
  return(parpop)
}
