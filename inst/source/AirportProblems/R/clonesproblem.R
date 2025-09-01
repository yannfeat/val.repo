#' Cost vector with cloned agents
#'
#' \code{clonesproblem} determines the new cost vector after disaggregating the original groups of clones.
#'
#' @param cw  A numeric cost vector, with the same length as \code{eta}.
#' @param eta A numeric vector representing the size of each group of cloned agents. All its elements must be positive integers.
#'
#' @return A numeric cost vector, where each element corresponds to a different agent's cost.
#'
#' @details
#' In an airport problem, agents \eqn{i,j \in N} are clones if both have the same cost parameter, that is, if \eqn{c_i=c_j}.
#'
#' If a problem has cloned agents, then the agent set \eqn{N} can be divided into several non-overlapping groups such that any
#' pair of agents that belong to the same group are clones, but any two agents from two different groups have different cost parameters.
#'
#' For each \eqn{t \in N}, let \eqn{T=\{1,\dots,t\}} and let \eqn{\mathcal{A}_t^N} be the set of pairs \eqn{(\eta, c)\in\mathbb{N}^t\times\mathbb{R}^t} such that:
#' \deqn{
#' \quad \space \space \space \space \eta=(\eta_1,\dots,\eta_t)\in \mathbb{N}^t \; \text{with} \; \eta_1+\dots+\eta_t=n}
#' \deqn{
#' c=(c_1,\dots,c_t) \in C^T \; \text{with} \; c_1<\dots< c_t
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
#' # 4 groups of cloned agents
#' cw <- c(2, 5, 7, 12) # Different costs
#' eta <- c(3, 2, 3, 1) # Size of each group of clones
#' clonesproblem(cw, eta) # General cost vector
#'
#' @seealso
#' \code{\link{clonesgroups}}
#'
#' @export
clonesproblem <- function(cw, eta) { # El problema del aeropuerto viene dado por grupos de clones
  # cw: Vector de costes sin agentes clonados (cada respectivo coste solo aparece una vez)
  # eta: Tamaño de cada grupo de agentes clonados

  # Verificación de que 'cw' es un vector numérico
  if (!is.numeric(cw)){
    stop("'cw' must be a numeric vector")
  }

  # Verificación de que 'eta' es un vector numérico
  if (!is.numeric(eta)){
    stop("'eta' must be a numeric vector")
  }

  # Comprobaciones:

  ## Requerimiento 1: Todos los costes del vector cw han de ser no negativos
  if (any(cw < 0)) {
    stop("'cw' must have nonnegative coordinates")
  }

  ## Requerimiento 2: Los componentes del vector eta han de ser números enteros positivos.
  if (any(floor(eta) != eta) || any(eta <= 0)) {
    stop("'eta' must be a vector of positive integers")
  }

  ## Requerimiento 3: Los vectores cw y eta han de tener la misma longitud
  if (length(cw) != length(eta)) {
    stop("'cw' and 'eta' must be the same length")
  }

  ## Aviso: El vector de costes cw no ha de incluir clones
  if (length(cw) != length(unique(cw))) {
    warning("'cw' has cloned agents")
  }

  n <- sum(eta)   # Número de agentes
  c <- numeric(n) # Se inicializa el vector de costes que sí incluye agentes clonados

  # Primer grupo de agentes clonados
  c[1:eta[1]] <- rep(cw[1], eta[1])

  # Grupos restantes de agentes clonados
  A <- cumsum(eta)
  for (ii in 2:length(cw)) {
    c[(A[ii - 1] + 1):A[ii]] <- rep(cw[ii], eta[ii])
  }

  # Se devuelve el vector de costes para todos los agentes
  return(c)
}
