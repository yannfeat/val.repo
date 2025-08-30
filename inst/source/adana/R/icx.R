# Improved Circular crossover (ICX)
icx = function(x1, x2, cxon, ...){
  m = length(x1)
  if(missing(cxon)) cxon = 2
  offsprings = matrix(NA, nrow=cxon, ncol=m)
  for(i in seq(from=1, to=cxon, by=2)){
    y1 = y2 = rep(NA, m)
    y1[1] = x2[1]
    y2[1] = x1[1]
    j1 = j2 = 1
    k1 = k2 = 2
    repeat{
      if(all(x1==x2)){  # If all genes are equal
        y1=y2=x1
        break
      }
      idx1 = which(x1==y1[j1])
      idx2 = which(x2==y2[j2])
      y1[k1] = x2[idx1]
      y2[k2] = x1[idx2]
      if(x1[1]==y1[k1] & k1<m){   # If there are incomplete genes
        remx1 = setdiff(x1, y1[!is.na(y1)])
        remx2 = setdiff(x2, y1[!is.na(y2)])
        y1[(k1+1):m] = remx2
        y2[(k2+1):m] = remx1
        break
      }
      j1 = k1
      j2 = k2
      k1 = k1+1
      k2 = k2+1
      if(k1>m) break
    }
    offsprings[i,] = y1
    if(i==cxon & cxon%%2==1) break
    offsprings[i+1,] = y2
  }
  return(offsprings)
}
