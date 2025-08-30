# Binary encoding of real number matrix
encodepop = function(x, lb, ub, eps, ...){
  n = nrow(x)
  if(missing(lb)) stop("lb, a lower boundary should be given.")
  if(missing(ub)) stop("ub, a upper boundary should be given.")
  if(missing(eps)) eps=0.1
  v = length(lb)
  if(length(eps)==1) eps = rep(eps, v)
  m = floor(log((ub-lb)/eps,2)+1)
  M = sum(m)
  binmat = matrix(NA, nrow=n, ncol=M)
  for(i in 1:n){
    binvec = c()
    for(j in 1:v){
      vbin = encode(x[i,j], lb[j], ub[j], m[j])
      binvec = c(binvec, vbin)
    }
    binmat[i,] = binvec
  }
  return(list(binmat=binmat, m=m))
}