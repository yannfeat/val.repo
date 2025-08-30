# Counter protected crossover (CPC)
cpc = function(x1, x2, cxon, ...){
  m = length(x1)
  if(missing(cxon)) cxon = 2
  offsprings = matrix(NA, nrow=cxon, ncol=m)
  lup = c(); ldown=c(); ll=0
  for(i in seq(from=1, to=cxon, by=2)){
    for(j in 1:m){
      if(x1[j]==1 & x2[j]==0){
        lup = c(lup, j)
        ll = ll+1
      }else if(x1[j]==0 & x2[j]==1){
        ldown = c(ldown, j)
      }
    }
    y1 = x1 ; y2 = x2
    for(j in ll){
      r = runif(1, 0, 1)
      if(r<0.5){
        temp = y1[lup[j]]
        y1[lup[j]] = y2[lup[j]]
        y2[lup[j]] = temp
        temp = y1[ldown[j]]
        y1[ldown[j]] = y2[ldown[j]]
        y2[ldown[j]] = temp
      }
    }
    offsprings[i,] = y1
    if(i==cxon & cxon%%2==1) break
    offsprings[i+1,] = y2
  }
  return(offsprings)
}
