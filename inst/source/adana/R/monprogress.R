# Monitor fitness value progress
monprogress = function(g, genfits, objective, ...){
  if(missing(g)) stop("generation number, g is missing")
  if(missing(genfits)) stop("generation fitness values, genfits is missing")
  if(missing(objective)) stop("objective is missing")
  if(objective=="min") genfits = -1*genfits
  plot(genfits[1:g,3], col=2, lwd=2, type="l",
       ylab="Fitness", xlab="Generation")
  title(main=paste("Iteration", g))
}
