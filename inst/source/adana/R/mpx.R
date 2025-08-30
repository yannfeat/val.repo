# Maximal protective crossover
mpx = function(x1, x2, cxon, ...){
  m = length(x1)
  if(missing(cxon)) cxon = 2
  offsprings = matrix(NA, nrow=cxon, ncol=m)
  for(i in seq(from=1, to=cxon, by=2)){
    y1 = y2 = rep(NA, m)
    j = m
    while(j > round(m/2)){
      v = sort(sample(1:m, size=2, replace=FALSE))
      j = length(v[1]:v[2])
    }
    k = j+1
    y1[1:j] = x1[v[1]:v[2]]
    y2[1:j] = x2[v[1]:v[2]]
    y1[k:m] = setdiff(x2, x1[v[1]:v[2]])
    y2[k:m] = setdiff(x1, x2[v[1]:v[2]])
    offsprings[i,] = y1
    if(i==cxon & cxon%%2==1) break
    offsprings[i+1,] = y2
  }
  return(offsprings)
}