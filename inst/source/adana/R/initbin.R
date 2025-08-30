# Initialize with binary encoding
initbin = function(n, m, prevpop, type, ...){
  if(missing(m)) m = 8
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
    if(nprev==0){ # Random initialization
      for(i in 1:n)  
        initpop[i, 1:m] = sample(0:1, m, replace=TRUE) 
    }else if(nprev<n){ # Hybrid initialization
      initpop[1:nprev,1:m] = prevpop[1:nprev,]
      for(i in (nprev+1): n) 
        initpop[i, 1:m] = sample(0:1, m, replace=TRUE) 
    }else if(nprev>n){ 
      initpop[1:n,1:m] = prevpop[1:n,]
    }else{ # Heuristic initialization
      initpop[1:nprev,1:m] = prevpop[1:nprev,]
    }
    initpop[,m+1] = 0
    if(nprev==0){
      rnames = paste0("T0", 1:n)
    }else if(nprev<n){
      rnames = c(paste0("Pre", 1:nprev), paste0("T0.", 1: (n-nprev)))
    }else{
      rnames = paste0("Pre", 1:n)
    }
    rownames(initpop) = rnames
    colnames(initpop) = c(paste0("gen", 1:m), "t", "fitval")
    if(type==2)
      initpop = initpop[,1:m]
    return(population=initpop)
}