# Displacement mutation
dismut = function(y, ...){
  n = length(y)
  v = sort(sample(1:(n-1), size=2, replace=FALSE))
  r = sample(1:n, 1)
  while(r==v[1]) r = sample(1:n, 1)
  ytemp = rep(NA,n)
  yrem = setdiff(1:n, v[1]:v[2])
  idx = r
  for(yval in y[v[1]:v[2]]){
    if(idx>n) idx=1
    ytemp[idx]= yval
    idx =idx + 1
  }
  idx=1
  for(yval in yrem){
    while(!is.na(ytemp[idx]))
      idx=idx+1
    ytemp[idx]=yval
    idx=idx+1
  }
  y=ytemp
  return(list(mutant=y, mutrange=v, r=r))
}