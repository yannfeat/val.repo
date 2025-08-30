# Blended crossover (BLX-alpha)
blxa = function(x1, x2, cxon, ...){
  m = length(x1)
  if(missing(cxon)) cxon = 2
  offsprings = matrix(NA, nrow=cxon, ncol=m)
  y1 = y2 = rep(NA, m)
  d = abs(x1-x2)
  for(i in seq(from=1, to=cxon, by=2)){
    for(j in 1:m){
      a = runif(1, 0, 1)
      y1[j] = runif(1, min(x1[j], 
                           x2[j])-a*d[j], max(x1[j], x2[j])+a*d[j])
      y2[j] = runif(1, min(x1[j], 
                           x2[j])-a*d[j], max(x1[j], x2[j])+a*d[j])
    }
    offsprings[i,] = y1
    if(i==cxon & cxon%%2==1) break
    offsprings[i+1,] = y2
  }
  return(offsprings)
}