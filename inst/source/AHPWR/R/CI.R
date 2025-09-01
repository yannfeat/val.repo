#'@title calculates saaty's consistency index
#'@name CI
#'@author Luciane Ferreira Alcoforado
#

#'@description  Function to calculate the  saaty's consistency index
#'
#'@param m is a matrice of pairwise comparison
#'@return Returns  saaty's consistency index
#'@examples
#'x=c("c1", "c2", "c3", "c4")
#'y=c(3, 9, 2, 8)
#'m=matrix_ahp(x,y)
#'CI(m)
#'
#'@examples
#'x=c("a1", "a2", "a3", "a4", "a5")
#'y=c(1, 9, 1.5, 8, 6)
#'m=matrix_ahp(x,y)
#'CI(m)
#'
#'@examples

#'m=diag(16)+2-2*diag(16)
#'m
#'CI(m)
#'CR(m)

#'@export

CI = function(m){
  #IR = c(0,0,0.58,0.9,1.12,1.24,1.32,1.41,1.45, 1.49, 1.51, 1.48, 1.56, 1.57, 1.59)
  n = length(m[1,])
  lambdamax=max(Re(eigen(m)$values))
IC = (lambdamax - n)/(n-1)
 return(IC)}


