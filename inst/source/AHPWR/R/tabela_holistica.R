#'@title organizes a table with comparison matrix data
#'@name tabela_holistica
#'@author Luciane Ferreira Alcoforado & Orlando Longo
#'
#'@description  Function to organizes a table with comparison matrix data
#'
#'@param pesos vector with holistic weights for comparison or comparison matrix data
#'
#'@return Returns a table with assigned holistic weights, comparison matrix, priority vector and consistency ratio
#'
#'@examples
#'pesos = c(3, 7, 9, 2)
#'names(pesos) = paste0("C",1:4)
#'tabela_holistica(pesos)
#'
#'@examples
#'m = matrix_ahp(y=c(3,4,3,2.5), x=paste0("A",1:4))
#'tabela_holistica(pesos=m)
#'
#'@import dplyr
#'
#'@export

tabela_holistica = function(pesos){
  #require(dplyr)
  if(is.vector(pesos)){
  if (is.null(names(pesos))) paste0("E",1:length(pesos))
  m2 = (matrix_ahp(x=names(pesos), y=pesos))
  l1=pesos
  l2 = autoVetor(m2); names(l2) = names(pesos)
  c2 = c(rep("_",length(pesos)+1),round(CR(m2),2))
  c1=c("weights", names(pesos),"priority")

  tabela = dplyr::bind_rows(pesos,as.data.frame(m2))
  tabela = dplyr::bind_rows(tabela,l2)
  tabela = dplyr::bind_cols(tabela,CR=c2)
  tabela = dplyr::bind_cols(c1,tabela)
  }

  if(is.matrix(pesos)){
  if (is.null(colnames(pesos))) colnames(pesos) = paste0("E",1:length(pesos))
  l2 = autoVetor(pesos); names(l2) = colnames(pesos)
  c2 = c(rep("_",length(l2)),round(CR(pesos),2))
  c1=c(colnames(pesos),"priority")

  tabela = dplyr::bind_rows(as.data.frame(pesos),l2)
  tabela = dplyr::bind_cols(tabela,CR=c2)
  tabela = dplyr::bind_cols(c1,tabela)
  row.names(tabela) <- NULL

  }
  return(tabela)

}
