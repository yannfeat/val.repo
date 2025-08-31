## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ---- eval=FALSE---------------------------------------------------------
#  # Initialize problem size and densities
#  # You can play around with these parameters
#  M=100 #number of rows of sampled matrix
#  N=200 #number of columns of sampled matrix
#  m=50 #number of rows of dense submatrix
#  n=40 #number of columns of dense submatrix
#  p=0.25 # noise density
#  q=0.85 #in-group density
#  
#  #Make binary matrix with mn-submatrix
#  random<-plantedsubmatrix(M=M, N=N,m=m,n=n,p=p,q=q)

## ---- eval=FALSE---------------------------------------------------------
#  
#  # Plot sampled G and matrix representations.
#  image(random$sampled_matrix, useRaster=TRUE, axes=FALSE, main = "Matrix A")
#  image(random$dense_submatrix, useRaster=TRUE, axes=FALSE, main = "Matrix X0")
#  image(random$disagreements, useRaster=TRUE, axes=FALSE, main = "Matrix Y0")

## ---- eval=FALSE---------------------------------------------------------
#  #Call ADMM solver
#  admm<-densub(G=random$sampled_matrix, m=m, n=n, tau = 0.35, gamma = 6/(sqrt(m*n)*(q-p)), opt_tol = 1.0e-4,maxiter=500, quiet = TRUE)
#  
#  
#  #Plot results
#  image(admm$X, useRaster=TRUE, axes=FALSE, main = "Matrix X")
#  image(admm$Y, useRaster=TRUE, axes=FALSE, main = "Matrix Y")
#  
#  

## ----jazz, eval=FALSE----------------------------------------------------
#  #Load dataset
#  load(file="JAZZ.RData")
#  
#  #Initialize problem size and densities
#  G=new #define matrix G equivalent to JAZZ dataset
#  m=100 #clique size or the number of rows of the dense submatrix
#  n=100 #clique size of the number of columns of the dense sumbatrix
#  tau=0.85 #regularization parameter
#  opt_tol=1.0e-2 #optimal tolerance
#  verbose=1
#  maxiter=2000 #number of iterations
#  gamma=8/n #regularization parameter
#  
#  
#  
#  #call ADMM solver
#  admm <- densub(G = G, m = m, n = n, tau = tau, gamma = gamma, opt_tol = opt_tol, maxiter=maxiter, quiet = TRUE)
#  # Planted solution X0.
#  X0=matrix(0L, nrow=198, ncol=198) #construct rank-one matrix X0
#  X0[1:100,1:100]=matrix(1L, nrow=100, ncol=100)#define dense block
#  
#  # Planted solution Y0.
#  Y0=matrix(0L, nrow=198, ncol=198) #construct matrix for counting disagreements between G and X0
#  Y0[1:100,1:100]=matrix(1L,nrow=100,ncol=1000)-G[1:100,1:100]
#  
#  #Check primal and dual residuals.
#  C=admm$X-X0
#  a=norm(C, "F") #Frobenius norm of matrix C
#  b=norm(X0,"F") #Frobenius norm of matrix X0
#  recovery = matrix(0L,nrow=1, ncol=1)#create recovery matrix
#  
#  if (a/b^2<opt_tol){
#  recovery=recovery+1
#  } else {
#    recovery=0 #Recovery condition
#    }
#  
#  
#  
#  
#  

