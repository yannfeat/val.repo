transformation.magnitude <- function(u){
  medval = median(u)
  if (medval == 0) logmed = 0 else logmed = floor(log10(abs(medval))) ;
  if (abs(logmed)>3){ fact = -logmed
  }else{fact = 0}
  utrans = u * 10^fact
  
  return(list(utrans = utrans, fact = fact))
}

