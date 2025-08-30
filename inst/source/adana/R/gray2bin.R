# Converting gray code to binary integer 1
gray2bin = function(gray){
  bin = rep(NA, length(gray))
  bin[1] = gray[1] 
  for(i in 2:length(bin))
    bin[i]= xor(gray[i], bin[i-1])
  return(bin)
}