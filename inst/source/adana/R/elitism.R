# Elistist replacement (elitism) function
elitism = function(parpop, offpop, reps, ...){
  if(missing(reps)) 
    reps=max(1, round(nrow(parpop)*0.05)) # Number of the best individuals
  parpop = parpop[order(parpop[,ncol(parpop)], decreasing=TRUE),]
  offpop = offpop[order(offpop[,ncol(offpop)], decreasing=TRUE),]
  offpop[(nrow(offpop)-reps+1):nrow(offpop),] = parpop[1:reps,]
  parrn = rownames(parpop[1:nrow(parpop),])
  offrn = rownames(offpop[1:(nrow(offpop)-reps),])
  unirn = c(offrn, parrn[1:reps])
  rownames(offpop) = unirn
  return(offpop)
} 