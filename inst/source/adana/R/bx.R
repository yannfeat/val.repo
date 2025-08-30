# straight cross 
bx = function(x1, x2, cxon, ...){
  m = length(x1)
  if(missing(cxon)) cxon=2
  offsprings = matrix(NA, nrow=cxon, ncol=m)
  for(i in 1:cxon){
    for(j in 1:m){
      selval = runif(1, min(x1[j], x2[j]), max(x1[j], x2[j]))
      offsprings[i,j] = selval
    }
    if(i==cxon & cxon%%2==1) break
  }
  return(offsprings)
}
