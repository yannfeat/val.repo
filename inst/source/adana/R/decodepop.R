# Converting binary number matrix to real number matrix
decodepop = function(x, lb, ub, m,  ...){
  n = nrow(x)
  if(missing(lb)) stop("lb, a lower boundary should be given.")
  if(missing(ub)) stop("ub, a upper boundary should be given.")
  if(missing(m)) stop("Chromosome length (m) is missing.")
  v = length(lb)
  xreal = matrix(NA, nrow=n, ncol=v)
  for(i in 1:n){
    realvec = c()
    for(j in 1:v){
      if(j==1){
        idx1 = 1
        idx2 = m[1]
      }else{
        idx1 = sum(m[1: (j-1)])+1
        idx2 = idx1+m[j]-1
      }
      vreal = decode(x[i,idx1:idx2], lb[j], ub[j], m[j])
      realvec = c(realvec, vreal)
    }
    xreal[i,] = realvec
  }
  return(xreal)
}