# Edge recombination
erx = function(x1, x2, cxon, ...){
  # Finding edges
  findedges = function(x){
    m = length(x)
    edges = list()
    for(i in 1:m){
      edge = c()
      if(i == 1)
        edge = c(edge, x[i+1], x[m])
      else if(i < m)
        edge = c(edge, x[i-1], x[i+1])
      else
        edge = c(edge, x[i-1], x[1])
      edges[[x[i]]]=edge
    }
    return(edges)
  }
  # Merge edges
  mergeedges = function(x1, x2){
    m = length(x1)
    edges = list()
    for(i in 1:m)
      edges[[i]]=sort(unique(c(x1[[i]], x2[[i]])))
    return(edges)
  }
  # Crossover
  m = length(x1)
  if(missing(cxon)) cxon = 2
  offsprings = matrix(NA, nrow=cxon, ncol=m)
  edges1 = findedges(x1)
  edges2 = findedges(x2)
  edges = mergeedges(edges1, edges2)
  for(i in seq(from=1, to=cxon, by=1)){
    tempedges = edges
    y = rep(NA, m)  
    idx = sample(1:m,1)
    y[1] = idx
    k = 2
    while(k<=m){
      for(j in 1:m)
        tempedges[[j]] = setdiff(tempedges[[j]], idx)
      nidx = Inf
      neighbors = tempedges[[idx]]
      for(j in neighbors){
        if(length(tempedges[[j]]) < nidx){
          nidx = length(tempedges[[j]])
          midx = j
        }
      }
      idx = midx
      y[k] = idx
      k = k + 1
    }
    offsprings[i,] = y
    if(i==cxon & cxon%%2==1) break
  }
  return(offsprings)
}