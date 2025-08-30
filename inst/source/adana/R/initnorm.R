# Normal distribution based initialization
initnorm = function(n, m, pmean, psd, type, ...){
  if(missing(m)) m = 8 # Gene count
  if(missing(n)) n = 4*m
  if(missing(pmean)) pmean = rep(0, m)
  if(missing(psd)) psd = rep(1, m)
  if(missing(type)) type = 1
  initpop = matrix(NA, ncol=m+2, nrow=n)
  for(i in 1:n)
    initpop[i,1:m] = rnorm(m, pmean, psd)
  initpop[,m+1] = 0
  rownames(initpop) = paste0("T0", 1:n)
  colnames(initpop) = c(paste0("gen", 1:m), "t", "fitval")
  if(type==2)
    initpop = initpop[,1:m]
  return(population=initpop)
}