# Adaptive Dynamic Algorithm (Adana 2)
adana2 = function(g, gmax, ...){
  pm = sqrt((g/gmax))
  pc = 1-pm
  return(list(pc=pc, pm=pm))
}
