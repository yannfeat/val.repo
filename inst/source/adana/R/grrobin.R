# Round Robin replacement function
grrobin = function(parpop, offpop, repk, ...){
  if(missing(repk)) repk = 10
  n = nrow(parpop)
  m = ncol(parpop)
  unipop = rbind(parpop, offpop)
  wins = rep(0, nrow(unipop))
  unipop = cbind(unipop, wins)
  idx = sample(1:nrow(unipop), replace=FALSE)
  for(i in idx){
    tidx = idx[-which(i==idx)]
    touridx = sample(tidx, size=repk, replace=FALSE)
    for(j in touridx){
      if(unipop[i,m] > unipop[j,m]){ 
        unipop[i,"wins"] = unipop[i,"wins"]+1
      }
    }
  }
  unipop = unipop[order(unipop[,"wins"], decreasing=TRUE),]
  unipop = unipop[1:n,1:m]
  return(unipop)
}
