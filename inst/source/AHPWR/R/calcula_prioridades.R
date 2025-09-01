#'@title Calculates the priority vector of a paired matrix
#'@name calcula_prioridades
#'@author Lyncoln Oliveira
#'@description Calculates the priority vector of a paired array based on a list
#'@param lista a paired matrix list
#'
#'@return Returns a list containing priority vectors for each matrix in the read list
#'
#'@export

calcula_prioridades = function(lista){
  prioridades = lapply(lista, function(x) autoVetor(x))
  return(prioridades)
}

