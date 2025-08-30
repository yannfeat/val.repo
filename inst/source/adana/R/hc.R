# Heuristic arithmetic crossover
hc = function(x1, x2, fitfunc, cxon, ...){
  m = length(x1)
  if(missing(cxon)) cxon = 2
  dargs = list(...)  
  offsprings = matrix(NA, nrow=cxon, ncol=m)
  x1fit = fitfunc(x1)
  x2fit = fitfunc(x2)
  for(i in seq(from=1, to=cxon, by=1)){
    r = runif(1, 0, 1)
    if(x2fit>=x1fit)
      y  = r * (x2-x1) + x2
    else
      y  = x1
    offsprings[i,] = y
    if(i==cxon & cxon%%2==1) break
  }
  return(offsprings)
}