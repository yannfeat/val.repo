# Convert binary vectors to integer vectors
decode4int = function(x, M, ...){
  nx = length(M)
  xint = integer(nx)
  j = 1 
  for(i in 1:nx){
    k = j+M[i]-1
    xint[i] = bin2int(x[j:k])
    j = k+1
  }
  return(xint)
}
