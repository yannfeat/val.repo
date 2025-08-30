# Multi-point crossover
kpx = function(x1, x2, cxon, cxk, ...){
  m = length(x1)
  if(missing(cxon)) cxon = 2
  if(missing(cxk)) cxk = 2
  offsprings = matrix(NA, nrow=cxon, ncol=m)
  for(i in seq(from=1, to=cxon, by=2)){
    v = sort(sample(1:(m-1), size=cxk, replace=FALSE)) 
    y1 = y2 = rep(NA, m)
    for(j in 1:v[1]){
      y1[j] = x1[j]
      y2[j] = x2[j]
    }
    direction = 1 
    for(j in 2:cxk){
      for(k in (v[j-1]+1):v[j]){
        if(direction){
          y1[k] = x2[k]
          y2[k] = x1[k]
        }else{
          y1[k] = x1[k]
          y2[k] = x2[k]
        }
      } 
      direction = ifelse(direction,0,1)
    }
    for(j in (v[cxk]+1):m){
      if(direction){
        y1[j] = x2[j]
        y2[j] = x1[j]
      }else{
        y1[j] = x1[j]
        y2[j] = x2[j]
      }
    }
    offsprings[i,] = y1
    if(i==cxon & cxon%%2==1) break
    offsprings[i+1,] = y2
  }
  return(offsprings)
}