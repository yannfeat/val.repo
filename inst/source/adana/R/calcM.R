# Calculate variable lengths
calcM = function(ub, ...){
  nx = length(ub)
  M = c()
  for(i in 1:nx)
    M = c(M, length(int2bin(ub[i])))
  return(M)
}
