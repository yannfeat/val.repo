# Select parents for the mating pool
select = function(selfunc, fitvals, ns, selb, selbc,
                  selc, selk, sells, selns, selps, sels, selt, 
                  selt0,selw, selg, selgmax, fmin, reptype, ...){
  args = as.list(match.call())[-1]
  nargs = length(args)
  if(missing(selfunc)) selfunc=seltour
  if(missing(reptype)) reptype=FALSE
  if(missing(ns)) ns=length(fitvals)
  if(missing(selw)) selw=2
  if(missing(selt)) selt=2
  if(missing(selb)) selb=0.5
  if(missing(selb)) selbc=0.5
  if(missing(selk)) selk=1.005
  if(missing(selc)) selc=0.5
  if(missing(sels)) sels=1.5
  if(missing(sells)) sells=1.5
  if(missing(selns)) selns=0.5
  if(missing(selps)) selps=0.5
  if(missing(selt0)) selt0=50
  selected = do.call(as.character(args[1]), args[2:nargs])
  return(selected)
}
