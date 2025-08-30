# Mu & Lambda replacement function
grmuvlambda = function(parpop, offpop, ...){
  n = nrow(parpop)
  offpop = offpop[order(offpop[, "fitval"], decreasing=TRUE),]
  offpop = offpop[1:n,]
  return(offpop)
}
