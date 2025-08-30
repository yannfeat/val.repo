# Find peaks and valleys in functions of one variable
findoptima = function(x, type="max", pflag=TRUE){
  if(type=="max"){
    if(pflag)
      results = which(diff(c(TRUE, diff(x)>=0,FALSE))<0)
    else
      results = which(diff(diff(x)>=0)<0)+1
  }else{
    if(pflag)
      results = which(diff(c(FALSE, diff(x)> 0, TRUE))>0)
    else
      results = which(diff(diff(x)>0)>0)+1
  }
  return(results)
}
