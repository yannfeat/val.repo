# Partially matched crossover
pmx = function(x1, x2, cxon, ...){
  m = length(x1)
  if(missing(cxon)) cxon = 2
  offsprings = matrix(NA, nrow=cxon, ncol=m)
  for(i in seq(from=1, to=cxon, by=2)){
    x = matrix(c(x1,x2), nrow=2, ncol=m, byrow=TRUE)
    y = matrix(NA, nrow=2, ncol=m)
    v = sort(sample(1:m, size=2))
    y[1:2,v[1]:v[2]] = x[2:1,v[1]:v[2]]
    for(j in setdiff(1:m, v)){
      if(!any(x[2,j] == y[1,v])){
        y[1,j] = x[2,j]
      }
      if(!any(x[1,j] == y[2,v])){
        y[2,j] = x[1,j] 
      }
    }
    y[1, is.na(y[1,])] = setdiff(x[2,], y[1,])
    y[2, is.na(y[2,])] = setdiff(x[1,], y[2,])
    offsprings[i,] = y[1,]
    if(i==cxon & cxon%%2==1) break
    offsprings[i+1,] = y[2,]
  }
  return(offsprings)
}