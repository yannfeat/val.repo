# Exchange/Link Crossover (EC,LC)
eclc = function(x1, x2, cxon, ...){
  m = length(x1)
  if(missing(cxon)) cxon = 2
  offsprings = matrix(NA, nrow=cxon, ncol=m)
  for(i in seq(from=1, to=cxon, by=1)){
    y = x2
    v = sort(sample(1:m, size=2, replace=FALSE))
    segx1 = x1[v[1]:v[2]]
    k = sample(1:m, 1)
    for(j in segx1){
      y[k] = j
      k = k+1
      if(k>m) k = 1
    }
    offsprings[i,] = y
    if(i==cxon & cxon%%2==1) break
  }
  return(offsprings)
}