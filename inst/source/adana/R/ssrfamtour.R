# Replacement function via family tournament
ssrfamtour = function(parpop, offpop, reppars, ...){
  if(missing(reppars)) reppars = c(1:2)
  n = nrow(parpop)
  m = ncol(parpop)
  parrn = rownames(parpop)
  offrn = rownames(offpop)
  fampop = rbind(parpop[reppars,], offpop)
  fampop = fampop[order(fampop[,m], decreasing=TRUE),]
  famrn = rownames(fampop)
  parpop[reppars,] = fampop[1:2,]
  parrn[reppars] = famrn[1:2]
  rownames(parpop)=parrn
  return(parpop)
}
