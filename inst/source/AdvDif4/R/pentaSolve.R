# Solve a pentadiagonal linear equation system
# Author: Jader Lugon Junior
# Date: 03/2017
# Version: 0.1.17
#
#' @export
pentaSolve <- function(a1,a2,a3,a4,a5,b)
{
u <- b
N <- length(b)
i <- 1
while(i<=N-2)
{
auxi <- a2[i]/a3[i]
a3[i+1] <- a3[i+1]-a4[i]*auxi
a4[i+1] <- a4[i+1]-a5[i]*auxi
b[i+1] <- b[i+1]-b[i]*auxi
auxi <- a1[i]/a3[i]
a2[i+1] <- a2[i+1]-a4[i]*auxi
a3[i+2] <- a3[i+2]-a5[i]*auxi
b[i+2] <- b[i+2]-b[i]*auxi
i <- i+1
}
auxi <- a2[N-1]/a3[N-1]
a3[N] <- a3[N]-a4[N-1]*auxi
b[N] <- b[N]-b[N-1]*auxi
# back substitution
u[N] <- b[N]/a3[N]
u[N-1] <- (b[N-1]-a4[N-1]*u[N])/a3[N-1]
i <- N-2
while(i>=1)
{
u[i] <- (b[i]-a4[i]*u[i+1]-a5[i]*u[i+2])/a3[i]
i <- i-1
}
return(u)
}
