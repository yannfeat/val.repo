# Order crossover (OX)
ox = function(x1, x2, cxon, ...){
  m = length(x1)
  if(missing(cxon)) cxon = 2
  offsprings = matrix(NA, nrow=cxon, ncol=m)
  for(i in seq(from=1, to=cxon, by=2)){
    x = matrix(c(x1,x2), nrow=2, ncol=m, byrow=TRUE)
    y = x
    v = sort(sample(2:(m-1), size=2, replace=TRUE))
    for(j in 1:2){
      part2 = x[j, v[1]:v[2]]
      revx <- x[j,-c(v[1]:v[2])]
      revx <- rev(revx)
      part1 = revx[1: (v[1]-1)]
      part3 = revx[(length(part1)+1):length(revx)] 
      y[i,] = c(part1, part2, part3)
    }
    offsprings[i,] = y[1,]
    if(i==cxon & cxon%%2==1) break
    offsprings[i+1,] = y[2,]
  }
  return(offsprings)
}