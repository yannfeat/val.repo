# Uniform partial match crossover
upmx = function(x1, x2, cxon, ...){
  m = length(x1)
  k = ceiling(m/3)
  if(missing(cxon)) cxon = 2
  offsprings = matrix(NA, nrow=cxon, ncol=m)
  for(i in seq(from=1, to=cxon, by=2)){
    y1 = x1; y2=x2
    j = 1
    while(j<=k){
      v1 = sample(1:m, 1) 
      gen1 = x2[v1]; gen2 = x1[v1]
      v21 = which(y1==gen1)
      v22 = which(y2==gen2)
      tmp = y1[v1]
      y1[v1] = y1[v21]
      y1[v21] = tmp
      tmp = y2[v1]
      y2[v1] = y1[v22]
      y2[v22] = tmp
      j = j+1
    }
    offsprings[i,] = y1
    if(i==cxon & cxon%%2==1) break
    offsprings[i+1,] = y2
  }
  return(offsprings)
}