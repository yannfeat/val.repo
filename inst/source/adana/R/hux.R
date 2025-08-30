# Heuristic uniform crossover
hux = function(x1, x2, cxon, cxps, ...){
  m = length(x1)
  if(missing(cxon)) cxon = 2
  if(missing(cxps)) cxps = 0.5
  offsprings = matrix(NA, nrow=cxon, ncol=m)
  for(i in seq(from=1, to=cxon, by=2)){
    y1 = x1
    y2 = x2
    ndg = 0  # Count of different genes
    for(j in 1:m){
      if(y1[j] != y2[j]){
        ndg = ndg +1
      }
    }
    sc = 0  # replacement counter
    while(sc <= ndg/2){
      for(j in 1:m){
        if(y1[j] != y2[j] & y1[j] != x2[j]){
          v = runif(1, 0, 1)
          if(v > cxps){
            y1[j] = x2[j]
            y2[j] = x1[j]
            sc = sc + 1
          }
        }
      }   
    }
    offsprings[i,] = y1
    if(i==cxon & cxon%%2==1) break
    offsprings[i+1,] = y2
  }
  return(offsprings)
}