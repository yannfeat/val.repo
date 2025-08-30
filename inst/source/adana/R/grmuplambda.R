# Mu+Lamda replacement function 1
grmuplambda = function(parpop, offpop, ...){
  n = nrow(parpop)
  unipop = rbind(parpop, offpop)
  unipop = unipop[order(unipop[, ncol(unipop)], decreasing=TRUE),]
  unipop = unipop[1:n,]
  return(unipop)
}