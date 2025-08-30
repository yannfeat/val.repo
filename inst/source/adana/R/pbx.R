# Position-based crossover (PBX)
pbx = function(x1, x2, cxon, ...){
  m = length(x1)
  if(missing(cxon)) cxon = 2
  offsprings = matrix(NA, nrow=cxon, ncol=m)
  for(i in seq(from=1, to=cxon, by=2)){
    x = matrix(c(x1,x2), nrow=2, ncol=m, byrow=TRUE)
    y = matrix(rep(NA, m), nrow=2, ncol=m)
    v = unique(sample(1:m, size=m, replace=TRUE))
    y[1,v] = x[2,v]
    y[2,v] = x[1,v]
    for(j in 1:2){
      cidx = which(is.na(y[j,]))
      sdif = setdiff(x[j,], y[j,v])
      y[j,cidx] = sdif
    }
    offsprings[i,] = y[1,]
    if(i==cxon & cxon%%2==1) break
    offsprings[i+1,] = y[2,]
  }
  return(offsprings)
}
