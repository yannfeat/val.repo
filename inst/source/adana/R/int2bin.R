# Convert integer to binary number
int2bin = function(int, m){
  int = round(int)
  mc = floor(log(int, base=2)+1)
  if(missing(m)) m = mc
  bin = rep(0, m)
  i = 0
  while(int >= 1){
    i = i + 1
    bin[i] = int %% 2
    int = int %/% 2
  }
  return(rev(bin))
}
