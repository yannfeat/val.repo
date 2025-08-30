# Adaptive Dynamic Algorithm (Adana 1)
adana1 = function(g, gmax, ...){
  pm = sin((g/gmax))+0.01
  pc = 1-pm
  return(list(pc=pc, pm=pm))
}