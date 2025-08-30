# Discrete Crossover
dc = function(x1, x2, cxon, cxps, ...){
  m = length(x1)
  if(missing(cxon)) cxon = 2
  if(missing(cxps)) cxps = 0.5
  offsprings = matrix(NA, nrow=cxon, ncol=m)
  y1 = x1; y2 = x2
  for(i in seq(from=1, to=cxon, by=2)){
    v = runif(1, 0, 1)
    for(j in 1:m){
      if(v >= cxps){
        y1[j] = x2[j]
        y2[j] = x1[j]
      }
    }
    offsprings[i,] = y1
    if(i==cxon & cxon%%2==1) break
    offsprings[i+1,] = y2
  }
  return(offsprings)
}