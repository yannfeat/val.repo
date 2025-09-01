#'@title creates a dataframe containing the judments holistic
#'@name matrix_ahp
#'@author Luciane Ferreira Alcoforado
#

#'@description  Function to build the judment matrix
#'
#'@param x is a vector of names criteria or choices
#'@param y is a vector of weigth scale Saaty, in [1,9]
#'@return Returns a judment matrice.
#'@examples
#'x=c("c1", "c2", "c3", "c4")
#'y=c(3, 9, 2, 8)
#'matrix_ahp(x,y)
#'
#'@examples
#'x=c("a1", "a2", "a3", "a4", "a5")
#'y=c(1, 9, 1.5, 8, 6)
#'matrix_ahp(x,y)

#'@export

matrix_ahp = function(x, y){
  n=length(x)
  m = diag(n)
  colnames(m) = x
  rownames(m) = x
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      m[i,j] = ifelse(y[i]<y[j], 1/(y[j]-y[i]+1),(y[i]-y[j]+1))
      m[j,i] = 1/m[i,j]
    }
  }
return(m)}

