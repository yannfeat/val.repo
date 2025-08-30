# Reduced parenting cross
rsc = function(x1, x2, cxon, ...){
  m = length(x1)
  if(missing(cxon)) cxon = 2
  offsprings = matrix(NA, nrow=cxon, ncol=m)
  for(i in seq(from=1, to=cxon, by=2)){
    pv = c() # Possible cuttingp points vector
    j = 1
    for(k in 1:m){
      if(x1[k] != x2[k]){
        pv[j] = k
        j = j + 1
      }
    }
    y1 = x1; y2 = x2 # offsprings
    if(length(pv)>0){
      v = sample(pv, size=1) # cutting point
      for(k in 1:m){
        if(k <= v){
          y1[k] = x1[k]
          y2[k] = x2[k]
        }else{
          y1[k] = x2[k]
          y2[k] = x1[k]
        }
      }
    }
    offsprings[i,] = y1
    if(i==cxon & cxon%%2==1) break
    offsprings[i+1,] = y2
  }
  return(offsprings)
}