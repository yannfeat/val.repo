#'@title  generates vectors of weights for criteria and alternatives
#'@name normaliza
#'@author Lyncoln Oliveira
#'

#'@description  Function that generates vectors of weights for criteria and alternatives
#'
#'@param lista is a list with judment matrices
#'@return Returns  auxiliary list
#'@examples
#'lista = list(M1=diag(3), M2=diag(3)+4-4*diag(3))
#'normaliza(lista)
#'


#'@export
normaliza = function(lista){
  #require(magrittr)
  lista_aux = list()
  for( i in 1:length(lista)){
    aux = lista[[i]] %>%  apply(2,sum)
    lista[[i]] = t(apply(lista[[i]], 1, function(x)x/aux))
    lista_aux[[i]] = apply(lista[[i]],1, sum)/length(lista[[i]][1,])
  }
  names(lista_aux) = names(lista)
  return(lista_aux)
}

