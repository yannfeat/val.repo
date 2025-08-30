# Random Selection With Resampling and Proportion
selrswrp = function(fitvals, ns, ...){
  if (missing(ns)) ns = length(fitvals) # Population size
  n = ns # Mating pool size
  fitprobs = fitvals/sum(fitvals)  # Fitness probabilities
  matpool = sample(1:n, size=ns, prob=fitprobs, replace=TRUE)
  return(matpool)
}