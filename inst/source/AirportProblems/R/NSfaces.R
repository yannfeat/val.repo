#' Face games associated with an airport problem
#'
#' \code{NSfaces} determines the airport problems defining the two components of the decomposition
#' of one specific face game.
#'
#' @param c A numeric cost vector.
#' @param R A numeric vector representing the agents forming the coalition
#'
#' @return A numeric matrix with two rows representing the decomposition of the \eqn{R}-face game:
#' \item{\code{[1,]}}{The first row is obtained by setting the cost of a specific coalition to zero while retaining the cost parameters of the complementary coalition.}
#' \item{\code{[2,]}}{The second row is derived by subtracting the highest cost in the complementary coalition from each agent's cost, or setting it to zero if the result is negative.}
#'
#' @details
#' Let \eqn{c\in C^N} be an airport problem and \eqn{v\in G^N} its associated cost game, for each non-empty proper coalition \eqn{R\in 2^N\backslash\{\emptyset, N\}}
#' define the \eqn{N\backslash R}-face of Core\eqn{(v)} as the set
#' \deqn{
#' F_{N\backslash R}(c)=\text{Core}(v)\cap\{x\in \mathbb{R}^N:x(R)=v(R)\}.
#' }
#' Also, the \eqn{N\backslash R}-face game \eqn{v_{F_{N\backslash R}}\in G^N} is given by \eqn{v_{F_{N\backslash R}}(S)=v(S\cup R)-v(R)+v(S\cap R),\ S\in 2^N}.
#' It turns out that \eqn{F_{N\backslash R}(c)=\text{Core}(v_{F_{N\backslash R}})}, the \eqn{N\backslash R}-face of the core of the associated cost game is the
#' core of the \eqn{N\backslash R}-face game. Let \eqn{r\in N} such that \eqn{c_r=v(R)=\text{max}\{c_i:i\in R\}} and denote \eqn{R_+=\{k\in N:k>r\}}.
#' Consider the airport problems
#' \deqn{
#' c_{|R}=(0_{N\backslash R}, c_R)\in C^N \text{ and } c_{|R_+}=(0_{N\backslash R_+},c_{r+1}-c_r,\dots,c_n-c_r)\in C^N
#' }
#' with associated games \eqn{v^{|R}\in G^N} and \eqn{v^{|R_+}\in G^N}, respectively. It is easy to see that the \eqn{N\backslash R}-face game
#' \eqn{v_{F_{N\backslash R}}\in G^N} is decomposable with respect to the partition \eqn{R,N\backslash R} and its components are
#' \eqn{v^{|R}} and \eqn{v^{|R_+}}. Moreover,
#' \deqn{
#' F_{N\backslash R}(c)=NS(c_R)\times NS(c_{r+1}-c_r,\dots,c_n-c_r)\times 0_{N\backslash(R\cup R_+)}.
#' }
#' Therefore, the components of the face games of the associated cost game are associated cost games themselves.
#' In the \eqn{N\backslash R}-face game, the players of \eqn{R} play the game associated with the problem where
#' the agents of \eqn{R} keep their cost parameters while the cost parameter of the agents in \eqn{N\backslash R} is null.
#' On the other hand, since the players of \eqn{R} already share \eqn{c_r=v(R)} among themselves, the players of
#' \eqn{N\backslash R} with an initial cost lower than \eqn{c_r} now have their cost parameter equal to zero while the others
#' see their cost reduced by \eqn{c_r}.
#'
#' @references
#' Bernárdez Ferradás, A., Mirás Calvo, M. Á., Quinteiro Sandomingo, C., and Sánchez-Rodríguez, E. (2025). Airport problems with cloned agents. [Preprint manuscript].
#'
#' González-Díaz, J. and Sánchez-Rodríguez, E. (2008). Cores of convex and strictly convex games. \emph{Games and Economic Behavior}, 62, 100-105.
#'
#' Mirás Calvo, M. Á., Quinteiro Sandomingo, C., and Sánchez-Rodríguez, E. (2020). The boundary of the core of a balanced game: faces games.
#' \emph{International Journal of Game Theory}, 49(2), 579-599.
#'
#' @examples
#' c <- c(1, 3, 7, 10) # Cost vector
#' R <- c(3, 4) # Coalition of agents
#' NSfaces(c, R) # Components of the face game
#'
#' @seealso
#' \code{\link{NScheck}}, \code{\link{NSstructure}}, \code{\link{NSset}}, \code{\link{hierarchicalrule}}
#'
#' @export
NSfaces <- function(c, R) { # Juego de las caras asociado al juego del aeropuerto
  # c es el vector de costes de los agentes
  # R es el vector de agentes que conforman la coalición ({1},{1,2},{1,2,3},...)

  # Verificación de que 'c' es un vector numérico
  if (!is.numeric(c)){
    stop("'c' must be a numeric vector")
  }

  # Verificación de que todos los costes sean no negativos (c => 0)
  if (any(c < 0)) {
    stop("c must have nonnegative coordinates")
  }

  n <- length(c)  # Número de agentes
  NR <- setdiff(1:n, R)  # Coalición complementaria a R (agentes que no están en R)

  # Validación de que R es una coalición válida
  if (any(!R %in% 0:n) || anyDuplicated(R)) {
    stop("R must be a subset of agents from 0 to n, without duplicated agents")
  }

  # Caso especial: R=0
  if (identical(R, 0)) {
    cR <- matrix(0, nrow=2, ncol=n)
    cR[2, ] <- c # La segunda fila es el vector de costes original
    return(cR)
  }

  # Se ordenan los costes en orden ascendente
  original.order <- order(c)
  c <- sort(c)
  end.order <- order(original.order)

  # Se recalcula NR y R en función del orden de los costes
  NR <- match(NR, original.order)
  R <- match(R, original.order)

  # Identificación del agente con mayor coste en NR
  if (length(NR) > 0) {
    r <- max(NR)  # Último agente en NR
  } else {
    r <- 0        # Si NR está vacío, r se asigna como 0
  }

  # Identificación de los agentes "más allá" de NR
  if (r + 1 > n) {
    NRplus <- integer(0)  # Vector vacío si r + 1 > n
  } else {
    NRplus <- (r + 1):n   # Secuencia normal si r + 1 <= n
  }

  # Construcción de los dos subproblemas (R-face)
  cR <- matrix(0, nrow = 2, ncol = n)
  cR[1, NR] <- c[NR]

  # Caso especial: R es la coalición completa (NR vacío)
  if (length(NR) == 0) {
    cR[2, ] <- c  # La segunda fila es el vector de costes original
    return(cR[, end.order])
  }

  # Resto de casos:
  if (length(NRplus) > 0) {
    cR[2, NRplus] <- c[NRplus] - c[r]
  }

  # Se restaura el orden original
  cR <- cR[, end.order]

  # Asignación de nombres para los agentes
  colnames(cR) <- 1:n

  return(cR)
}
