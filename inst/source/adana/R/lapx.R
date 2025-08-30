# Laplace crossover
lapx = function(x1, x2, cxon, cxa, cxb, ...){
  m = length(x1)
  if(missing(cxon)) cxon = 2
  if(missing(cxa)) cxa = 0.0
  if(missing(cxb)) cxb = 0.15
  if(length(cxa) == 1) cxa = rep(cxa, m)
  if(length(cxb) == 1) cxb = rep(cxb, m)
  offsprings = matrix(NA, nrow=cxon, ncol=m)
  for(i in seq(from=1, to=cxon, by=2)){
    x = matrix(c(x1,x2), nrow=2, ncol=m, byrow=TRUE)
    y = matrix(NA, nrow=2, ncol=m)
    pr1 = runif(m)
    pr2 = runif(m)
    beta = cxa + ifelse(pr1 > 0.5, cxb*log(pr2), -cxb*log(pr2))
    bp = beta*abs(x[1,] - x[2,])
    y[1,] = pmin(pmax(x[1,] + bp, range(x[1,])[1]), range(x[1,])[2])
    y[2,] = pmin(pmax(x[2,] + bp, range(x[2,])[1]), range(x[2,])[2])
    offsprings[i,] = y[1,]
    if(i==cxon & cxon%%2==1) break
    offsprings[i+1,] = y[2,]
  }
  return(offsprings)
}