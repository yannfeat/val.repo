#' Coalitional game associated with an airport problem
#'
#' \code{airportgame} computes the coalitional game for cost-sharing in an airport problem.
#'
#' @param c A numeric cost vector.
#' @param lex A logical value indicating the output order of the game. By default, \code{lex = TRUE} returns the game in lexicographic order. However, if \code{lex = FALSE}, the game is returned in binary order.
#'
#' @return A numeric vector representing the associated coalitional game.
#'
#' @details
#' Let \eqn{N = \{1, \dots, n\}} denote the set of agents, and let \eqn{c \in \mathbb{R}_+^N} be the cost vector such that
#' \eqn{c \geq 0}. The value \eqn{c_i} should be interpreted as the associated cost for each agent \eqn{i} in the context of
#' the problem, i.e., every component represents the cost of the facility required by an agent.
#' Segmental costs are defined as the difference between a given cost and the first immediately lower cost: \eqn{c_i - c_{i-1}} for \eqn{i \in N \backslash \{1\}}.
#' Therefore, \eqn{C^N} represents the domain of all problems.
#'
#' Given an airport problem \eqn{c \in \mathbb{R}^N_{+}}, the corresponding coalitional game is defined, for each \eqn{S \subseteq N}, as:
#'
#' \deqn{
#' v(S) = \max\{c_j : j \in S\}.
#' }
#'
#' It is easy to check that this class of games associated with airport problems is always concave, since for any pair of coalitions
#' \eqn{S \subseteq T \subseteq N}, it is verified that:
#'
#' \deqn{
#' v(S \cup \{i\}) - v(S) \geq v(T \cup \{i\}) - v(T) \quad \text{for all} \quad i \notin T
#' }
#'
#' An efficient way to represent a nonempty coalition \eqn{S \in 2^N} is by identifying it with the binary sequence
#' \eqn{a_n,a_{n-1},\dots a_1}, where \eqn{a_i=1} if \eqn{i \in S} and \eqn{a_i=0} otherwise. Consequently, each coalition
#' \eqn{S} is represented by the number associated with its binary representation: \eqn{\sum_{i \in S} 2^{i-1}}.
#' Then coalitions can be ordered by their associated numbers.
#'
#' Alternatively, coalitions can be ordered lexicographically, meaning they are first sorted by increasing size,
#' and then by lexicographic order among coalitions of the same size.
#'
#' @references
#' Littlechild, S. C. and Owen, G. (1973). A simple expression for the Shapley value in a special case.
#' \emph{Management Science}, 23, 370-372.
#'
#' Thomson, W. (2024). Cost allocation and airport problems.
#' \emph{Mathematical Social Sciences}, 31(C), 17–31.
#'
#' @examples
#' # 4 agents
#' (c <- c(1, 3, 7, 10)) # Vector of costs
#' airportgame(c, lex = TRUE)  # Game in lexicographic order
#' airportgame(c, lex = FALSE) # Game in binary order
#'
#' @seealso
#' \code{\link{airportvector}}
#'
#' @importFrom utils combn
#'
#' @export
airportgame <- function(c, lex = TRUE) {
  # airportgame: Juego coalicional asociado a un problema de aeropuerto
  # lex: Si "lex" es TRUE, se devuelve el juego en orden lexicográfico. Si "lex" es FALSE, se devuelve el juego en orden binario.

  # Verificación de que 'c' es un vector numérico
  if (!is.numeric(c)){
    stop("'c' must be a numeric vector")
  }

  # Verificación de que todos los costes sean no negativos (c >= 0)
  if (any(c < 0)) {
    stop("'c' must have nonnegative coordinates")
  }

  # Verificación de que 'lex' es válido
  if (!is.logical(lex) || length(lex) != 1) {
    stop("'lex' must be a single logical value (TRUE or FALSE)")
  }

  n <- length(c)  # Número de agentes

  # Construcción del juego en orden binario
  if (lex == FALSE) {
    if (all(c == sort(c))) {# Los costes vienen dados en orden creciente
      v <- c(c[1])
      for (ii in 2:n) {
        v <- c(v, rep(c[ii], 2^(ii-1)))
      }
    }
    else {
      v <- numeric(2^n - 1)
      for (S in 1:(2^n - 1)) {
        players <- which(intToBits(S)[1:n] == 1)
        v[S] <- max(c[players])
      }
    }
    return(v)
  }

  # Construcción del juego en orden lexicográfico
  else {
    coalitions <- unlist(lapply(1:n, function(k) combn(1:n, k, simplify = FALSE)), recursive = FALSE)
    v <- numeric(length(coalitions))
    for (i in seq_along(coalitions)) {
      P <- coalitions[[i]]
      v[i] <- max(c[P])  # Se obtiene el valor máximo entre los jugadores de la coalición
    }
    return(v)
  }
}
