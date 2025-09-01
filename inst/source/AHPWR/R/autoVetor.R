#'@title Calculates the eigen vector of matrix
#'@name autoVetor
#'@author Lyncoln Oliveira
#'@description Calculates the eigen vector of matrix
#'@param matriz a paired matrix
#'
#'@return Returns a normalized eigenvector
#'
#'@examples

#'m=diag(16)+2-2*diag(16)
#'m
#'autoVetor(m)
#'
#'@export
#'
autoVetor = function(matriz){
  #Achando o autovetor associado ao maior autovalor
  autoValores = Re(eigen(matriz)$values)
  autoVetores = Re(eigen(matriz)$vectors)
  autoValorMax = which.max(autoValores)
  autoVetorAssociado = autoVetores[,autoValorMax]
  autoVetorNormalizado = autoVetorAssociado/sum(autoVetorAssociado)

  return(autoVetorNormalizado)}
