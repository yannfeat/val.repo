# Extended line crossover (ELX)
elx = function(x1, x2, lb, ub, cxon, cxealfa, ...){
  m = length(x1)
  if(missing(cxon)) cxon = 2
  if(missing(cxealfa)) cxealfa = 1
  offsprings = matrix(NA, nrow=cxon, ncol=m)
  v1 = -Inf; v2=Inf
  for(i in seq(from=1, to=cxon, by=1)){
    y = rep(NA, m)
    for(j in 1:m){
      if(x1[j]!=x2[j]){
        tl = (lb[j]-x2[j])/(x1[j]-x2[j])
        tu = (ub[j]-x2[j])/(x1[j]-x2[j])
        tmin = min(tl, tu)
        tmax = max(tl, tu)
        v1 = max(v1,tmin)
        v2 = min(v2,tmax)
      }   
    }
    lambda = runif(1, max(v1,-cxealfa), min(v2, 1+cxealfa))
    y = lambda * x1 + (1-lambda)*x2
    offsprings[i,] = y
    if(i==cxon & cxon%%2==1) break
  }
  return(offsprings)
}