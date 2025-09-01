#'@title calculates saaty's consistency ratio
#'@name CR
#'@author Luciane Ferreira Alcoforado
#

#'@description  Function to calculate the  saaty's consistency ratio
#'
#'@param m is a matrice of pairwise comparison
#'@return Returns  saaty's consistency ratio in [0,1]
#'@examples
#'x=c("c1", "c2", "c3", "c4")
#'y=c(3, 9, 2, 8)
#'m=matrix_ahp(x,y)
#'CR(m)
#'
#'@examples
#'x=c("a1", "a2", "a3", "a4", "a5")
#'y=c(1, 9, 1.5, 8, 6)
#'m=matrix_ahp(x,y)
#'CR(m)
#'
#'@examples

#'m=diag(16)+2-2*diag(16)
#'m
#'CI(m)
#'CR(m)

#'@export

CR = function(m){
  IR = c(0,0,0.58,0.9,1.12,1.24,1.32,1.41,1.45, 1.49, 1.51, 1.48, 1.56, 1.57, 1.59)
  n = length(m[1,])
  IC = CI(m)
  if(n>15) n=15
  RC=ifelse(n>2,IC/IR[n],0)
 return(RC)}

