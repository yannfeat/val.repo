# Converting from binary to gray code integer
bin2gray = function(bin){
  gray = rep(NA, length(bin))
  gray[1] = bin[1]
  for(i in 2:length(bin))
    gray[i]= xor(bin[i], bin[i-1])
  return(gray)
}