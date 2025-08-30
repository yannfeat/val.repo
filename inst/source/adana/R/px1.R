# One-point crossover
px1 = function(x1, x2, cxon, ...){
  m = length(x1)
  if(missing(cxon)) cxon = 2
  offsprings = matrix(NA, nrow=cxon, ncol=m)
  for(i in seq(from=1, to=cxon, by=2)){
    v = sample(0:(m-1), size=1)  # Cutting point
    if(v == 0){
      y1 = x2
      y2 = x1
    }else if(v>0 & v<m){
      y1 = c(x1[1:v], x2[(v+1):m])
      y2 = c(x2[1:v], x1[(v+1):m])
    }
    offsprings[i,] = y1
    if(i==cxon & cxon%%2==1) break
    offsprings[i+1,] = y2
  }
  return(offsprings)
}