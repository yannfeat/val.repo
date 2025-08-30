# Disrespectful Crossover (DISC)
disc = function(x1, x2, cxon, ...){
  m = length(x1)
  if(missing(cxon)) cxon = 2
  offsprings = matrix(NA, nrow=cxon, ncol=m)
  for(i in seq(from=1, to=cxon, by=1)){
    y = rep(NA, m)
    v = sample(1:m, 1)
    for(j in 1:v){
      if(x1[j] != x2[j])
        y[j] = x1[j]
      else
        y[j] = sample(0:1, 1)
    }
    for(j in (v+1):m){
      if(x1[j] != x2[j])
        y[j] = x2[j]
      else
        y[j] = sample(0:1, 1)
    }
    offsprings[i,] = y
    if(i==cxon & cxon%%2==1) break
  }
  return(offsprings)
}