# Crossover
cross = function(crossfunc, matpool, cxon, cxpc, gatype, ...){
  func = as.character(match.call()[2])
  if(missing(gatype)) gatype="gga"
  if(missing(cxpc)) cxpc=0.95  # Proportion of crossover
  if(missing(cxon)) cxon=2     # Offspring number per mating
  dargs = list(...)
  dargs$cxon = cxon
  nm = nrow(matpool) # Number of individuals in the mating pool
  mm = ncol(matpool) # Number of genes
  nc = round(nm*cxpc) 
  nc = ifelse(nc%%2==1, nc-1, nc)
  nc = ifelse(nc>=nm, nm-1, nc) # Number of individuals to mate
  nc = ifelse(nc<2, 2, nc) # Number of individuals to mate
  if(gatype=="gga")  
    offsprings = matrix(NA, nrow=(nc/2*cxon), ncol=mm) # Offsprings
  else
    offsprings = matrix(NA, nrow=cxon, ncol=mm) # Offsprings
  mc = mm-2
  i = 1
  for(j in seq(from=1, to=nc-1, by=2)){
    dargs$x1 = unname(matpool[j,1:mc])
    dargs$x2 = unname(matpool[j+1,1:mc])
    crossresult = do.call(func, dargs)
    for(k in 1:cxon){
      offsprings[i,1:mc] = crossresult[k,]
      i=i+1	
    }
    if(gatype=="ssga"){  # Return if SSGA
      return(offsprings)
    }
  }
  if(nc<nm)
    offsprings = rbind(offsprings, matpool[(nc+1):nm,])
  return(offsprings)
}