#'@title  Create paired matrix and can test saaty consistency rate
#'@name matriz_julgamento
#'@author Lyncoln Oliveira
#

#'@description  Function that Create paired matrix and can test saaty consistency rate
#'
#'@param n_comp Number of elements to be evaluated
#'@param CR If TRUE also returns the consistency rate of saaty, if FALSE returns only matrix
#'@param n_matrix Number of matrix to be created
#'
#'@return Returns a list with 2 positions. First position contains the paired matrices and the second position their consistency rates
#'
#'@export

matriz_julgamento = function(n_comp,CR = TRUE, n_matrix = 1){
  matrizes = list()
  erros = c()
  conjunto = list()
  for(k in 1:n_matrix){
  if(n_matrix != 1) print(paste0("fill the matrix ",as.character(k)))
  matriz = diag(1, n_comp ,n_comp)
    for (i in 1:(n_comp-1)){
      for(j in (i+1):(n_comp)){
      valor = eval(parse(text = (readline(paste0("How important is the criterion? ",as.character(i)," in relation to the criterion ",as.character(j),": ")))))
      matriz[i,j] = valor
      matriz[j,i] = 1/valor
      }
    }
    matrizes[[k]] = matriz
    if(CR == TRUE) erros[k] = CR(matriz)
  }

  conjunto[[1]] = matrizes; names(conjunto) = "Matrix"
  if(CR == TRUE) {conjunto[[2]] = erros; names(conjunto) = c("Matrix","CR")}
  return(conjunto)
}
