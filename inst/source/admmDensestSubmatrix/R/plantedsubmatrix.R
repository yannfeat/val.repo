#' Sample matrix
#'
#' Generates binary \eqn{(M,N)} - matrix sampled from dense \eqn{(m,n)} - submatrix.
#'
#' Let \eqn{U*} and \eqn{V*} be \eqn{m} and \eqn{n} index sets.
#' For each i in U*, j in V* we let \eqn{a_ij = 1} with probability \eqn{q} and \eqn{0}  otherwise.
#' For each remaining \eqn{ij} we set \eqn{a_ij = 1} with probability \eqn{p < q} and take \eqn{a_ij = 0} otherwise.
#'
#' @param M number of rows in sampled matrix
#' @param N number of rows in sampled matrix
#' @param m number of rows in dense submatrix
#' @param n natural number used to calculate number of rows in dense submatrix
#' @param p density outside planted submatrix
#' @param q density inside planted submatrix
#' @return Matrix \eqn{G} sampled from the planted dense \eqn{(mn)}-submatrix model, dense sumbatrix \eqn{X0}, matrix \eqn{Y0} used to count the number of disagreements between \eqn{G} and \eqn{X0}
#' @export
#' @examples
#' plantedsubmatrix(10,10,1,2,0.25,0.75)

plantedsubmatrix <- function(M,N,m,n,p,q){

  gamma<-3/((q-p)*m*n)

  ##Initialize G
  G <- matrix(runif(M*N),M,N)
  G <- ceiling(G-(1-p))

  ##Make dense submatrix
  start_position <- matrix(runif(m*n),m,n)
  G[1:m, 1:n]  <- ceiling(start_position-(1-q))

  ##Get X, fill the dense submatrix
  X0 <- matrix(0L, nrow=M, ncol=N)
  X0[1:m, 1:n] <- matrix(1L, nrow=m, ncol=n)

  ##Get Y
  Y0 <- matrix(0L, nrow=M, ncol=N)
  Y0[1:m,1:n] <- matrix(1L, nrow=m,ncol=n) - G[1:m,1:n]

  my_list<-list(sampled_matrix = G, dense_submatrix = X0, disagreements = Y0) #G is sampledmatrix, X0 is dense, Y0 is number of disagr
  return (my_list)

}






