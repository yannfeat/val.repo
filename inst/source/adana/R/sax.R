# Single arithmetic crossover 
sax = function(x1, x2, cxon, cxalfa, ...){
  m = length(x1)
  if(missing(cxon)) cxon = 2
  if(missing(cxalfa)) cxalfa = runif(1, 0, 1)
  offsprings = matrix(NA, nrow=cxon, ncol=m)
  for(i in seq(from=1, to=cxon, by=2)){
    y1 = y2 = rep(NA, m)
    v = sample(1:m, 1)
    y1[1:v] = x1[1:v]
    y2[1:v] = x2[1:v]
    y1[(v+1):m] = (1-cxalfa) * x2[(v+1):m]
    y2[(v+1):m] = (1-cxalfa) * x1[(v+1):m]
    offsprings[i,] = y1
    if(i==cxon & cxon%%2==1) break
    offsprings[i+1,] = y2
  }
  return(offsprings)
}