# Circular crossover (CX)
cx = function(x1, x2, cxon, ...){
  m = length(x1)
  if(missing(cxon)) cxon = 2
  offsprings = matrix(NA, nrow=cxon, ncol=m)
  for(i in seq(from=1, to=cxon, by=2)){
    y1 = y2 = rep(NA, m)
    j = 1
    y1[j] = x1[j]
    y2[j] = x2[j]
    repeat{
      k = which(x1==x2[j])
      y1[k] = x2[j]
      y2[k] = x2[k]
      j = k
      if( (x2[j] %in% y1))
        break
    }
    for(j in 1:m){
      if(is.na(y1[j])){
        y1[j] = x2[j]
      }
      if(is.na(y2[j])){
        y2[j] = x1[j]
      }
    }
    offsprings[i,] = y1
    if(i==cxon & cxon%%2==1) break
    offsprings[i+1,] = y2
  }
  return(offsprings)
}
