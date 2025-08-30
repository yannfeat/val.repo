# Swap + Inversion mutation
invswapmut = function(y, ...){
  n = length(y)
  vswap = sort(sample(1:n, size=2, replace=FALSE))
  takas = y[vswap[1]] 
  y[vswap[1]] = y[vswap[2]]
  y[vswap[2]] = takas
  vinv = sort(sample(1:n, size=2, replace=FALSE))
  subgenes = y[vinv[1]:vinv[2]] 
  subgenes = rev(subgenes)
  y[vinv[1]:vinv[2]] = subgenes
  return(list(mutant=y, mutgen=unique(c(vswap,vinv))))
}