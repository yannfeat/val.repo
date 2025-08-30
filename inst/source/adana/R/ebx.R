# enhanced box cross
ebx = function(x1, x2, lb, ub, cxon, cxalfa, ...){
  m = length(x1)
  if(missing(cxon)) cxon=2
  if(missing(cxalfa)) cxalfa = runif(1, 0, 1)
  offsprings = matrix(NA, nrow=cxon, ncol=m)
  for(i in 1:cxon){
    for(j in 1:m){
      minx = min(x1[j],x2[j])
      maxx = max(x1[j],x2[j])
      emin = minx - cxalfa*(maxx-minx)
      emax = maxx + cxalfa*(maxx-minx)
      offsprings[i,j] = runif(1, min(emin,lb[j]), max(emax, ub[j]))
    }
  }
  return(offsprings)
}