#'@title Calculates the AHP General
#'@name ahp_geral
#'@author Lyncoln Oliveira
#'@description Calculates AHP in a list of paired arrays or in a properly formatted excel worksheet stipend.

#'@param objeto List of paired arrays or excel path containing the properly formatted paired arrays.
#'@param mapeamento Vector containing the number of subscriptions of each criteria, from left to right. If not filled the pattern and fill with 0. If in doubt, see the tutorial vignette.
#'@param nomes_alternativas Vector containing the names of the alternatives in your hierarchy,
#'if not filled returns a vector of LETTERS\[1\:qtdAlternatives\]
#'
#'@return Table containing the relationships of criteria, subscriptions (If any) and Alternatives Using the AHP system.
#'
#'
#'@examples
#'m1=matrix(c(1, 1/5, 3, 1/5, 1/3,5, 1, 5, 3, 3,
#' 1/3, 1/5, 1, 1/3, 1/3,5, 1/3, 3, 1, 1,3, 1/3,
#' 3, 1, 1),ncol=5,byrow=TRUE)

#'m2=matrix(c(1, 1/3, 1/6, 3, 1, 1/2,6, 2, 1),nrow=3, byrow=TRUE)
#'m3=matrix(c(1, 1/2, 1/2,2, 1, 2, 2, 1/2, 1),nrow=3, byrow=TRUE)
#'m4=matrix(c(1, 1, 2,1, 1, 1, 1/2, 1, 1),nrow=3, byrow=TRUE)
#'m5=matrix(c(1, 2, 3,1/2, 1, 2, 1/3, 1/2, 1),nrow=3, byrow=TRUE)
#'m6=matrix(c(1, 5, 3,1/5, 1, 1/3, 1/3, 3, 1),nrow=3, byrow=TRUE)
#'base=list(m1,m2,m3,m4,m5,m6)
#'mapeamento=rep(0,5)
#'nomes_alternativas="PADRAO"
#'ahp_geral(base,mapeamento, nomes_alternativas)


#'@export
#'
#'

ahp_geral = function(objeto, mapeamento = "PADRAO", nomes_alternativas = "PADRAO"){
  if(is.character(objeto)) base = ler(objeto)
  else base = objeto
  if(mapeamento[1] == "PADRAO") mapeamento = rep(0,dim(base[[1]])[1])
  if(nomes_alternativas[[1]] == "PADRAO") nomes_alternativas = LETTERS[1:dim(base[[length(base)]])[1]]
  tabela = ahp(base, mapeamento,nomes_alternativas)
  #tabela1 = ahp_s(base, map=mapeamento,nomes_alternativas)
 # result= list(tb1=tabela, tb2=tabela1)

  return(tabela)
}


