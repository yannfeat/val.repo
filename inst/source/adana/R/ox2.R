# Order-based crossover (OX2)
ox2 = function(x1, x2, cxon, cxoxk, ...){
  m = length(x1)
  if(missing(cxon)) cxon = 2
  if(missing(cxoxk)) cxoxk = max(1, sample(2:round(m/2),1))
  offsprings = matrix(NA, nrow=cxon, ncol=m)
  for(i in seq(from=1, to=cxon, by=2)){
    v = sample(1:m, size=cxoxk, replace=FALSE)
    y1 = x2
    gv = x1[v]
    idx = c()
    for(k in gv)
      idx = c(idx, which(k==y1))
    y1[rev(idx)] = gv
    y2 = x1
    gv = x2[v]
    idx = c()
    for(k in gv)
      idx = c(idx, which(k==y2))
    y2[rev(idx)] = gv
    offsprings[i,] = y1
    if(i==cxon & cxon%%2==1) break
    offsprings[i+1,] = y2
  }
  return(offsprings)
}