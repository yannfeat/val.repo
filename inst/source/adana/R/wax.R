# Whole arithmetic crossover 
wax = function(x1, x2, cxon, cxalfa, ...){
  n = length(x1)
  if(missing(cxon)) cxon = 2
  offsprings = matrix(NA, nrow=cxon, ncol=n)
  for(i in seq(from=1, to=cxon, by=2)){
    if(missing(cxalfa)) cxalfa = runif(1, 0, 1)
    y1 = cxalfa * x1 + (1-cxalfa) * x2
    y2 = (1-cxalfa) * x1 + cxalfa * x2
    offsprings[i,] = y1
    if(i==cxon & cxon%%2==1) break
    offsprings[i+1,] = y2
  }
  return(offsprings)
}
