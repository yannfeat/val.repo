# Converting gray code to binary integer 2
gray2bin2 = function(gray){
  bin = rep(NA, length(gray))
  bin[1] = bitvalue = gray[1] 
  for(i in 2:length(bin)){
    if(gray[i]==1) bitvalue=ifelse(bitvalue==1, 0, 1)
    else bitvalue=gray[i-1]
    bin[i]= bitvalue
  }
  return(bin)
}
