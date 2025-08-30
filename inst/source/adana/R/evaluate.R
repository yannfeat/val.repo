# Calculate the fitness values of population 
evaluate = function(fitfunc, population, objective, ...){
  if(missing(fitfunc)) stop("The fitness function is missing.")
  if(missing(population)) stop("Population is missing.")
  if(missing(objective)) objective="max"
  if(!is.matrix(population)) 
    population = matrix(population, 1, length(population))
  dotargs = list(...)
  fitvals = rep(NA, nrow(population))
  for(i in 1:nrow(population))
    fitvals[i] = fitfunc(population[i,], dotargs)
  if(objective=="min") 
    fitvals = -1*fitvals
  return(fitvals)
}
