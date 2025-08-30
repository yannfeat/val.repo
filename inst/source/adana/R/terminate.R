# Termination control function
terminate = function(tercrit, maxiter, objective, t, genfits,
                     fitvals, objval, optdif, rmcnt, rmdif, abdif, mincv,
                     sddif, rangedif, simlev, phidif, meandif, bestdif,
                     stime, maxtime){
  if(missing(optdif)) optdif=1e-06
  if(missing(rmcnt)) rmcnt=10
  if(missing(rmdif)) rmdif=1e-06
  if(missing(abdif)) abdif=1e-06
  if(missing(phidif)) phidif=1e-04
  if(missing(meandif)) meandif=1e-06
  if(missing(bestdif)) bestdif=1e-03
  if(missing(sddif)) sddif=1e-03
  if(missing(rangedif)) rangedif=1e-03
  if(missing(mincv)) mincv=0.05
  if(missing(simlev)) simlev=0.975
  if(missing(maxtime)) 
    maxtime=ifelse(maxiter>=1e06, 3e-04*maxiter, 60)
  terminate = 0
  # Maximum number of iterations
  if(is.element(1, tercrit)){
    if(t > maxiter) terminate = 1
  }
  # Reaching the global optimum (objective value)
  if(is.element(2,tercrit)){
    if(!missing(objval)){
      if(genfits[t,2]==objval) terminate = 2
    }
  }
  # Reaching or getting very close to the global optimum
  if(is.element(3,tercrit)){
    if(!missing(objval)){
      optdifc = abs(objval-genfits[t,2])
      if(optdifc <= optdif) terminate = 3
    }
  }
  # Minimum difference between last k best fitness mean
  if(is.element(4, tercrit)){
    if(t > rmcnt){
      tb = t-rmcnt+1 
      rmean = mean(genfits[tb:t,2])
      cbestdif = abs(rmean-genfits[t,2])
    }else{
      cbestdif = Inf
    }
    if(cbestdif <= bestdif) terminate = 4
  }
  # Minimum difference between the last two fitness averages
  if(is.element(5, tercrit)){
    cmeandif = Inf
    if(t > 1) 
      cmeandif = abs(genfits[t,3]-genfits[(t-1),3])
    if(cmeandif <= meandif) terminate = 5
  }
  # Minimum difference between the last k fitness means
  if(is.element(6, tercrit)){
    if(t > rmcnt){
      tb = t-rmcnt+1 
      rmean = mean(genfits[tb:t,3])
      crmdif = abs(rmean-genfits[t,3])
    }else{
      crmdif = Inf
    }
    if(crmdif <= rmdif) terminate = 6
  }
  # Minimum difference between average and best fitness
  if(is.element(7, tercrit)){
    conv = abs(genfits[t,2]-genfits[t,3])
    if(conv <= abdif) terminate = 7
  }
  # Minimum standard deviation difference
  if(is.element(8, tercrit)){
    if(t > 1){
      csddif = abs(genfits[t,4] - genfits[t-1,4])
      if(csddif <= sddif) terminate = 8
    }
  }
  # Minimum and maximum difference
  if(is.element(9, tercrit)){
    crangedif = abs(genfits[t,1]-genfits[t,2])
    if(crangedif <= rangedif) terminate = 9
  }
  # Minimum coefficient of variation
  if(is.element(10, tercrit)){
    cv = ifelse(genfits[t,3]==0, 
                Inf, genfits[t,4]/genfits[t,3])
    if(abs(cv) <= mincv) terminate = 10
  }
  # Phi convergence
  if(is.element(11, tercrit)){
    phi = ifelse(genfits[t,2]==0, 
                 1, 1-abs(genfits[t,3]/genfits[t,2]))
    if(phi <= phidif) terminate = 11
  }
  # Percentage of fitness similarity
  if(is.element(12, tercrit)){
    csimlev = (length(fitvals)-length(unique(fitvals))) /
      length(fitvals)
    if(csimlev >= simlev) terminate = 12
  }
  # Time out
  if(13 %in% tercrit){
    timedif = as.difftime(Sys.time()-stime, units="mins")
    timedif = as.numeric(timedif, units = "mins")
    if(timedif > maxtime) terminate = 13
  }
  return(terminate)
}
