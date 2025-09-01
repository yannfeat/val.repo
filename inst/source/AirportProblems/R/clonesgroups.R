#' Cloned agents in an airport problem
#'
#' \code{clonesgroups} determines a new cost vector that excludes cloned agents, and calculates the size
#' of each clone group that shares the same cost.
#'
#' @param c A numeric cost vector.
#'
#' @return A list containing the following items:
#' \tabular{lllll}{
#' \code{cw} \tab \tab \tab   A numeric cost vector, with the same length as \code{eta}. \cr
#' \code{eta}\tab \tab \tab  A numeric vector representing the size of each group of cloned agents. \cr
#' }
#'
#' @details
#' In an airport problem, agents \eqn{i,j \in N} are clones if both have the same cost parameter, that is, if \eqn{c_i=c_j}.
#'
#' If a problem has cloned agents, then the agent set \eqn{N} can be divided into several non-overlapping groups such that any
#' pair of agents that belong to the same group are clones, but any two agents from two different groups have different cost parameters.
#'
#' For each \eqn{t \in N}, let \eqn{T=\{1,\dots,t\}} and let \eqn{\mathcal{A}_t^N} be the set of pairs \eqn{(\eta, c)\in\mathbb{N}^t\times\mathbb{R}^t} such that:
#' \deqn{
#' \quad \space \space \eta=(\eta_1,\dots,\eta_t)\in \mathbb{N}^t \;\text{with}\; \eta_1+\dots+\eta_t=n}
#' \deqn{
#' c=(c_1,\dots,c_t) \in C^T \;\text{with}\;  c_1<\dots< c_t
#' }
#' Given \eqn{t\in N} and \eqn{(\eta,c)\in \mathcal{A}_t^N} we define the cost problem \eqn{\eta\ast c \in C^N} as:
#' \deqn{
#' \eta\ast c=(\eta_1\ast c_1,\dots,\eta_t\ast c_t)=(c_1,\overset{\eta_1}{\dots},c_1,\dots,c_t,\overset{\eta_t}{\dots},c_t)\in C^N
#' }
#' Given a problem \eqn{d\in C^N} there are unique \eqn{t\in N} and \eqn{(\eta,c)\in \mathcal{A}_t^N} such that \eqn{d=n\ast c}.
#' We refer to the problem \eqn{c\in C^T}, formed by the different cost parameters of problem \eqn{d\in C^N}, as the reduced problem without clones
#' associated with \eqn{d\in C^N}. Clearly, \eqn{d\in C^N} is obtained from \eqn{c\in C^T} by adding, for each \eqn{i\in T}, \eqn{\eta_i}, clones of agent \eqn{i}.
#'
#' Let \eqn{t\in N} and \eqn{(\eta,c)\in \mathcal{A}_t^{\mathcal{N}}}. For each \eqn{s\in T=\{1,\dots,t\}}, let \eqn{M_s^\eta=\eta_1+\dots+\eta_s} and, to simplify the notation,
#' write \eqn{N^\eta_s=N^{\eta\ast c}_s=\{j\in N:(\eta\ast c)_j=c_s\}}. Therefore, \eqn{N_1^\eta=\{1,\dots,M^\eta_1\}} and \eqn{N_s^\eta=\{M^\eta_{s-1}+1,\dots,M^\eta_s\}} if \eqn{s\in T\backslash\{1\}}.
#' Obviously, the family \eqn{\{N^\eta_1,\dots,N^\eta_t\}} is a partition of \eqn{N} and \eqn{|N^\eta_s|=\eta_s} for all \eqn{s\in T}. Moreover, all the agents that belong to \eqn{N^\eta_s} have
#' the same cost parameter \eqn{c_s}, i.e., \eqn{(\eta\ast c)_j=c_s} for all \eqn{s\in T} and \eqn{j\in N^\eta_s}.
#' So, each agent in the reduced problem \eqn{c\in C^T} can be seen as a representative agent of the corresponding group of clones in the original problem \eqn{d=\eta\ast c \in C^N}.
#'
#' @references
#' Bernárdez Ferradás, A., Mirás Calvo, M. Á., Quinteiro Sandomingo, C., and Sánchez-Rodríguez, E. (2025). Airport problems with cloned agents. [Preprint manuscript].
#'
#' Littlechild, S. C. and Thompson, G. F. (1977). Aircraft landing fees: a game theory approach.
#' \emph{The Bell Journal of Economics}, 8, 186-204.
#'
#' @examples
#' # 9 different agents
#' c <- c(2, 2, 2, 5, 5, 7, 7, 7, 12) # Cost vector
#' clonesgroups(c) # 4 groups of cloned agents
#'
#' @seealso
#' \code{\link{clonesproblem}}
#'
#' @export
clonesgroups <- function(c) {

  # Verificación de que 'c' es un vector numérico
  if (!is.numeric(c)){
    stop("'c' must be a numeric vector")
  }

  # # Verificación de que todos los costes sean no negativos (c=>0)
  if (any(c < 0)) {
    stop("'c' must have nonnegative coordinates")
  }

  # Vector de costes sin agentes clonados
  cw <- unique(c) # Valores únicos de c

  # Vector que almacena el número de agentes clonados en cada grupo
  eta <- numeric(length(cw))  # Se inicializa el vector eta

  # Recuento de los agentes clonados en cada grupo
  for (ii in seq_along(cw)) {
    eta[ii] <- sum(c == cw[ii])
  }

  # Se devuelven los vectores cw y eta
  return(list(cw = cw, eta = eta))
}
