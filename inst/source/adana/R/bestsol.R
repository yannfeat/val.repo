# Best solution monitoring function
bestsol = function(garesult){
  chromosome = garesult$bestsol$chromosome
  fitval = garesult$bestsol$fitval
  generation = garesult$bestsol$generation
  cat("Best solution\n")
  cat(" Chromosome:", chromosome,"\n")
  cat(" Fitness:", fitval,"\n")
  cat(" Generation:", generation,"\n")
}