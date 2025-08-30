# Modified partial match crossover
mpmx = function(x1, x2, cxon, ...){
  m = length(x1)
  if(missing(cxon)) cxon = 2
  offsprings = matrix(NA, nrow=cxon, ncol=m)
  for(i in seq(from=1, to=cxon, by=2)){
    v = sort(sample(2: (m-1), size=2)) 
    y1 = y2 = rep(NA, m)
    # Middle part
    y1[v[1]:v[2]] = x1[v[1]:v[2]]
    y2[v[1]:v[2]] = x2[v[1]:v[2]]
    # Left part
    for(j in 1: (v[1]-1)){
      if(!(x2[j] %in% y1)) y1[j] = x2[j]
      if(!(x1[j] %in% y2)) y2[j] = x1[j]
    }
    # Rigth part
    for(j in (v[2]+1):m){
      if(!(x2[j] %in% y1)) y1[j] = x2[j]
      if(!(x1[j] %in% y2)) y2[j] = x1[j]
    }
    # random permutation
    y1[is.na(y1)] = sample(setdiff(x2, y1[!is.na(y1)])) 
    y2[is.na(y2)] = sample(setdiff(x1, y2[!is.na(y2)]))
    offsprings[i,] = y1
    if(i==cxon & cxon%%2==1) break
    offsprings[i+1,] = y2
  }
  return(offsprings)
}