# Dynamic mutation and crossover function (Adana 3)
adana3 = function(fitvals, g, gmax, cxpc, mutpm,
                  adapc, adapd, ...){
  if(missing(adapc)) adapc = 0.05
  if(missing(adapd)) adapd = 0.05
  fmin = min(fitvals)
  fmax = max(fitvals)
  favg = mean(fitvals)
  newrate = ifelse(g/gmax>=0.01, g/gmax,0.01)
  pca = ifelse((fmax-favg)/fmax <= adapc, newrate, cxpc)
  pma = ifelse((fmax-favg)/fmax <= adapd, newrate, mutpm)
  return(list(pc=pca, pm=pma))
}
