# Convert integer vectors to binary vectors
encode4int = function(x, M, ...){
  nx = length(x)
  xbin = c()
  for(i in 1:nx)
    xbin = c(xbin, int2bin(x[i], m=M[i]))
  return(xbin)
}
