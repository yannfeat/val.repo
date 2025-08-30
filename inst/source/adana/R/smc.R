# Sinus movement crossover (SMC)
smc = function(x1, x2, cxon, ...){
  m = length(x1)
  if(missing(cxon)) cxon = 2
  offsprings = matrix(NA, nrow=cxon, ncol=m)
  for(i in seq(from=1, to=cxon, by=2)){
    y1 = y2 = rep(NA, m)
    y1[1] = x1[1]
    j = 1
    k1 = 2
    k2 = 1
    repeat{
      if(x2[j] %in% y1[!is.na(y1)]){
        y2[k2]=x2[j]
        k2 = k2+1
      }else{
        y1[k1]=x2[j]
        k1 = k1+1
      }
      j = j+1
      if(x1[j] %in% y1[!is.na(y1)]){
        y2[k2]=x1[j]
        k2 = k2+1
      }else{
        y1[k1]=x1[j]
        k1 = k1+1
      }
      if(j==m){
        y2[m] = x2[m]
        break
      }
    }
    offsprings[i,] = y1
    if(i==cxon & cxon%%2==1) break
    offsprings[i+1,] = y2
  }
  return(offsprings)
}
