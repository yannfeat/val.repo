#'@title Transforms table with numbers into percentage with 2 decimal places
#'@name transforma_tabela
#'@author Lyncoln Oliveira
#'
#'@description  Function to Transforms table with numbers into percentage with 2 decimal places
#'
#'@param tabela table to transforms
#'
#'@return Returns a transform table with percentage
#'
#'@examples
#'tabela=data.frame(x=c(0.5, 0.25), y=c(0.55, 0.93))
#'transforma_tabela(tabela)
#'
#'@import dplyr
#'@export

transforma_tabela = function(tabela){
  #require(dplyr)
  numero_linhas = dim(tabela)[1]
  numero_colunas = dim(tabela)[2]

  tabela_porcento = dplyr::mutate_if(tabela, is.numeric, function(x) paste0(round(100*x,2),"%"))
  #tabela_porcento = dplyr::slice(tabela_porcento, numero_linhas, 1:(numero_linhas - 1))
  #nomes_criterios = c(tabela_porcento$Criterios[1], unlist(lapply(tabela_porcento$Criterios[2:numero_linhas],function(x) paste0("-  ",x))))
  #tabela_porcento = dplyr::mutate(tabela_porcento)

  return(tabela_porcento)

}
