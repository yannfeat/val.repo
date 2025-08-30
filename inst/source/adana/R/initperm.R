# Permutation coded initialization
initperm = function(n, permset, prevpop, type, ...){
  if(missing(permset)) stperm=sample(0:9, 10, replace=FALSE)
  m = length(permset) # Gene count
  if(missing(prevpop)) prevpop = NULL
  if(is.null(prevpop)){
    nprev = 0
  }else{
    prevpop = as.matrix(unname(prevpop))
    nprev = nrow(prevpop)
    m = ncol(prevpop)
  }
  if(missing(n))
    if(is.null(prevpop)) n = 4*m else n=nprev
  if(missing(type)) type = 1
  initpop = matrix(NA, ncol=m+2, nrow=n) # Initial population
  if(nprev==0){
    for(i in 1:n) 
      initpop[i,1:m] = sample(permset, m, replace=FALSE)
  }else if(nprev<n){
    initpop[1:nprev,1:m] = prevpop[1:nprev,]
    for(i in (nprev+1): n) 
      initpop[i,1:m] = sample(permset, m, replace=FALSE)
  }else if(nprev>n){
    initpop[1:n,1:m] = prevpop[1:n,]
  }else{
    initpop[1:nprev,1:m] = prevpop[1:nprev,]
  }
  initpop[,m+1] = 0
  if(nprev==0){
    rnames = paste0("T0.", 1:n)
  }else if(nprev<n){
    rnames = c(paste0("Pre.", 1:nprev), 
               paste0("T0", 1:(n-nprev)))
  }else{
    rnames = paste0("Pre.", 1:n)
  }
  rownames(initpop) = rnames
  colnames(initpop) = c(paste0("gen", 1:m), "t", "fitval")
  if(type==2)
    initpop = initpop[,1:m]
  return(population=initpop)
}