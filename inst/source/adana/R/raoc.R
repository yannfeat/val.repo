# Random And/Or Crossover (RAOC)
raoc = function(x1, x2, cxon, ...){
  m = length(x1)
  if(missing(cxon)) cxon = 2
  offsprings = matrix(NA, nrow=cxon, ncol=m)
  y1 = y2 = rep(NA, m)
  alfa = runif(1, 0, 1)
  for(i in seq(from=1, to=cxon, by=2)){
    for(j in 1:m){
      r = runif(1, 0, 1)
      if(r>alfa){
        y1[j]= ifelse(x1[j] & x2[j],1,0)
        y2[j]= ifelse(x1[j] | x2[j],1,0)
      }else{
        y1[j]= ifelse(x1[j] | x2[j],1,0)
        y2[j]= ifelse(x1[j] & x2[j],1,0)
      }
    }
    offsprings[i,] = y1
    if(i==cxon & cxon%%2==1) break
    offsprings[i+1,] = y2
  }
  return(offsprings)
}