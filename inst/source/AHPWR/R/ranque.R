#'@title Calculates the ranking of alternatives
#'@name ranque
#'@author Lyncoln Oliveira
#'@description Calculates ranking of alternatives for a list of judment matrix
#'@param tabela table building by ahp_s or ahp_geral

#'@return Table containing the ranking of alternatives
#'
#'
#'@examples
#'x=paste0(letters[3],1:5)
#'y=c(5,2,7,3,2)
#'m1=matrix_ahp(x,y)
#'x=paste0(letters[1],1:3)
#'y=c(4.4,5.2,3)
#'m2=matrix_ahp(x,y)
#'y=c(2,4,3)
#'m3=matrix_ahp(x,y)
#'y=c(4.9,5,3.3)
#'m4=matrix_ahp(x,y)
#'y=c(4.4,4.2,4.3)
#'m5=matrix_ahp(x,y)
#'y=c(5.4,5.2,5.7)
#'m6=matrix_ahp(x,y)
#'base=list(m1, m2, m3, m4, m5, m6)
#'mapeamento = rep(0,5)
#'nomes_alternativas = paste0(letters[1],1:3)
#'tabela = ahp(base,mapeamento, nomes_alternativas)
#'ranque(tabela)
#'
#'#with subcriteria and 3 criteria and 2 alternatives
#'mapeamento = c(2,0,0) #2 subcriteria in criteria 1 and 0 subcriteria to others
#'x=paste0(letters[3],1:3) #3 criteria
#'y=c(5,2,7)
#'m1=matrix_ahp(x,y) #compare criteria
#'x=paste0(letters[4],1:2)
#'y=c(4,6)
#'m2=matrix_ahp(x,y) # 2 compare 2 subcriteria of criteria 1
#'x=paste0(letters[1],1:2)
#'y=c(2,4)
#'m3=matrix_ahp(x,y) #alternatives for subcriteria 1
#'y=c(4.9,5)
#'m4=matrix_ahp(x,y) #alternatives for subcriteria 2
#'y=c(4.4,4.2)
#'m5=matrix_ahp(x,y) #alternatives for criteria 2
#'y=c(5.4,5.2)
#'m6=matrix_ahp(x,y) ##alternatives for criteria 3

#'base=list(m1, m2, m3, m4, m5, m6)
#'
#'nomes_alternativas = paste0(letters[1],1:2)
#'tabela = ahp(base,mapeamento, nomes_alternativas)
#'ranque(tabela)

#'
#'@import dplyr
#'@import tidyr
#'
#'@export
ranque = function(tabela){
  num_colunas = length(tabela[1,])
  nun_linhas = length(tabela[,1])
  alternativas = tabela[1,3:(num_colunas-1)]
  return(dplyr::select(dplyr::mutate(dplyr::arrange(tidyr::gather(alternativas,Alternativas,Pesos),desc(Pesos)),Ranque = c(1:length(alternativas[1,]))),Ranque,dplyr::everything()))
}
