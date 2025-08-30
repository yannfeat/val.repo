# Blended crossover (Alfa-Beta)
blxab = function(x1, x2, cxon, ...){
  n = length(x1)
  if(missing(cxon)) cxon = 2
  offsprings = matrix(NA, nrow=cxon, ncol=n)
  for(i in seq(from=1, to=cxon, by=2)){
    y1 = y2 = rep(NA, n)
    d = abs(x1-x2)
    for(j in 1:n){
      if(x1[j] <= x2[j]){
        alfa = runif(1, 0, 1)
        beta = runif(1, 0, 1)
        y1[j] = runif(1, x1[j]-alfa*d[j], x2[j]+beta*d[j])
        alfa = runif(1, 0, 1)
        beta = runif(1, 0, 1)
        y2[j] = runif(1, x1[j]-alfa*d[j], x2[j]+beta*d[j])
      }else{
        alfa = runif(1, 0, 1)
        beta = runif(1, 0, 1)
        y1[j] = runif(1, x2[j]-beta*d[j], x2[j]+alfa*d[j])
        alfa = runif(1, 0, 1)
        beta = runif(1, 0, 1)
        y2[j] = runif(1, x2[j]-beta*d[j], x2[j]+alfa*d[j])
      }
    }
    offsprings[i,] = y1
    if(i==cxon & cxon%%2==1) break
    offsprings[i+1,] = y2
  }
  return(offsprings)
}
