# Local arithmetic crossover
lax = function(x1, x2, cxon, ...){
  n = length(x1)
  if(missing(cxon)) cxon = 2
  offsprings = matrix(NA, nrow=cxon, ncol=n)
  for(i in seq(from=1, to=cxon, by=2)){
    x = matrix(c(x1, x2), nrow=2, ncol=n, byrow=TRUE)
    y = matrix(NA, nrow=2, ncol=n)
    cxalfa = runif(n) # Alpha is different for each gene
    y[1,] = cxalfa*x[1,] + (1-cxalfa)*x[2,]
    y[2,] = cxalfa*x[2,] + (1-cxalfa)*x[1,]
    offsprings[i,] = y[1,]
    if(i==cxon & cxon%%2==1) break
    offsprings[i+1,] = y[2,]
  }
  return(offsprings)
}