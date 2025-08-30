# Random Respectful Crossover (RRC)
rrc = function(x1, x2, cxon, ...){
  m = length(x1)
  if(missing(cxon)) cxon = 2
  offsprings = matrix(NA, nrow=cxon, ncol=m)
  for(i in seq(from=1, to=cxon, by=1)){
    y = x1 & x2
    v = which(x1!=x2)
    for(j in v)
      y[j] = ifelse(runif(1)>0.5, 1, 0)
    offsprings[i,] = y
    if(i==cxon & cxon%%2==1) break
  }
  return(offsprings)
}