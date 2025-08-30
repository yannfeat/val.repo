# ILM/DHC adaptation function
ilmdhc = function(g, gmax, ...){
  pmg = g/gmax
  pcg = 1-pmg
  return(list(pc=pcg, pm=pmg))
}
