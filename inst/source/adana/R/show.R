# Function to visualize iteration results
show = function(monitorfunc, g, genfits, objective, x, ...){
  args = as.list(match.call())[-1]
  nargs = length(args)
  if(missing(monitorfunc)) monitorfunc=monprogress
  if(missing(g)) stop("Parameter g is missing")
  if(missing(genfits)) stop("Parameter genfits is missing")
  if(missing(objective)) stop("Parameter objective is missing")
  if(missing(x)) stop("Parameter x is missing")
  do.call(as.character(args[1]), args[2:nargs])
}