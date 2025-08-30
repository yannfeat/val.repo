# mixed crossover
sc = function(x1, x2, cxon, ...){
  m = length(x1)
  if(missing(cxon)) cxon = 2
  offsprings = matrix(NA, nrow=cxon, ncol=m)
  sidx = sample(1:m, size=m) # mixing operation
  for(i in seq(from=1, to=cxon, by=2)){
    v = sample(1: (m-1), 1)   # Cutting point
    y1 = y2 = rep(NA, m) # offsprings
    for(j in 1:m){
      if(j < v) {
        y1[sidx[j]] = x1[sidx[j]]
        y2[sidx[j]] = x2[sidx[j]]
      }else{
        y1[sidx[j]] = x2[sidx[j]]
        y2[sidx[j]] = x1[sidx[j]]
      }
    }
    offsprings[i,] = y1
    if(i==cxon & cxon%%2==1) break
    offsprings[i+1,] = y2
  }
  return(offsprings)
}