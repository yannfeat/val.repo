# Convert from real number to binary number
encode = function(real, lb, ub, m){
  num = (real-lb)*(2^m-1)/(ub-lb)
  bin = int2bin(num, m)
  return(bin)
}