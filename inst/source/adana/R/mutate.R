# Mutation application function
mutate = function(mutfunc, population, mutpm, gatype, ...){
  func = as.character(match.call()[2])
  dotargs = list(...)
  n = nrow(population)
  m = ncol(population)-2
  if(missing(gatype)) gatype="gga"
  if(missing(mutpm)) mutpm=0.05
  nm = round(n*mutpm)  # Number of individuals mutated
  if(gatype=="ssga"){
    if(runif(1,0,1) <= mutpm) nm=1 else nm=0
  }
  if(nm>0){
    midx = sample(1:n, size=nm, replace=FALSE) # Mutant indices
    for(i in midx){
      dotargs$y = unname(population[i,1:m])
      population[i,1:m] = do.call(func, dotargs)$mutant
    }
  }
  return(population)
}