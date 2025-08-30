# Asymmetric Two-Point Crossover (ATC)
atc = function(x1, x2, cxon, ...){
  m = length(x1)
  if(missing(cxon)) cxon = 2
  offsprings = matrix(NA, nrow=cxon, ncol=m)
  for(i in seq(from=1, to=cxon, by=2)){
    v = c()
    y1 = y2 = rep(NA, m)
    v = sort(sample(1:m, 2, replace=FALSE))
    v[3] = sample(1:m, 1)
    j = v[3]
    for(k in 1:m){
      if(k<v[1])
        y1[k] = x1[j]
      else if(v[1]<=k & k<=v[2]){
        y1[k] = x2[j]
        j = j+1
        if(j>m) j=1
      }else{
        y1[k] = x1[k]
      }
    }
    for(k in 1:m){
      if(k<v[1])
        y2[k] = x2[k]
      else if(v[1]<=k & k<=v[2]){
        y2[k] = x1[k]
      }else{
        y2[k] = x2[k]
      }
    }
    offsprings[i,] = y1
    if(i==cxon & cxon%%2==1) break
    offsprings[i+1,] = y2
  }
  return(offsprings)
}
