# MAXONE fitness function 1
maxone1 = function(x, ...){
  sum1=0
  for(bits in x){
    if(bits) sum1=sum1+1  
  }
  return(sum1)
}