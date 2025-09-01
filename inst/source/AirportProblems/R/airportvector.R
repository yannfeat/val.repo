#' Cost vector associated with an airport game
#'
#' \code{airportvector} computes the cost vector corresponding to an airport problem.
#'
#' @param v A numeric vector that represents the characteristic function of the airport game.
#' @param lex A logical value indicating the input order of the game. By default, if \code{lex = TRUE}, the game has been introduced in lexicographic order. However, if \code{lex = FALSE}, the game has been established in binary order.
#'
#' @return A numeric vector representing the cost for each agent of an airport game.
#'
#' @details
#' A cooperative game \eqn{(N, v)} is considered an airport game provided that its characteristic function \eqn{v} satisfies:
#' \deqn{
#' v(S)=\text{max}\{c_j:j \in S\}, \quad \forall S \subseteq N,S \neq \emptyset
#' }
#' where \eqn{c_j} represents the individual cost associated with each agent \eqn{j}.
#'
#' Evidently, this property implies that the cost assigned to a coalition is determined by the most expensive cost for its members.
#' It is for this reason that this class of games is always concave.
#'
#' The airport game can be given in lexicographic order or binary order. For instance, if
#' \eqn{n=3}, the characteristic function of the associated airport game in lexicographic order is:
#' \deqn{
#' v=[v(\{1\}),v(\{2\}),v(\{3\}),v(\{1,2\}),v(\{1,3\}),v(\{2,3\}),v(\{1,2,3\})]
#' }
#' On the other hand, in binary order, it would be:
#' \deqn{
#' v=[v(\{1\}),v(\{2\}),v(\{1,2\}),v(\{3\}),v(\{1,3\}),v(\{2,3\}),v(\{1,2,3\})]
#' }
#' Anyway, in both cases, we have that \eqn{v(\{2\})=v(\{1,2\})} and \eqn{v(\{3\})=v(\{1,3\})=v(\{2,3\})=v(\{1,2,3\})}.
#'
#' Given an airport game \eqn{(N, v)}, it is possible to extract the corresponding cost vector \eqn{c} by setting:
#' \deqn{
#' c_j=\text{min}\{v(S):j\in S \},\quad \text{for all} \quad j \in N.
#' }
#'
#' @references
#' Littlechild, S. C. and Owen, G. (1973). A simple expression for the Shapley value in a special case.
#' \emph{Management Science}, 23, 370-372.
#'
#' Thomson, W. (2024). Cost allocation and airport problems.
#' \emph{Mathematical Social Sciences}, 31(C), 17–31.
#'
#' @examples
#' # 4 agents in lexicographic order
#' v <- c(1, 3, 7, 10, 3, 7, 10, 7, 10, 10, 7, 10, 10, 10, 10)
#' airportvector(v, lex = TRUE)
#'
#' # 4 agents in binary order
#' u <- c(1, 3, 3, 7, 7, 7, 7, 10, 10, 10, 10, 10, 10, 10, 10)
#' airportvector(u, lex = FALSE)
#'
#' @seealso
#' \code{\link{airportgame}}
#'
#' @importFrom utils combn
#'
#' @export
airportvector <- function(v, lex = TRUE) {
  # Vector de costes asociado a un juego del aeropuerto
  # lex: Si "lex" es TRUE, el juego original viene dado en orden lexicográfico. Si "lex" es FALSE, el juego viene en orden binario.

  n <- log2(length(v) + 1) # Número de jugadores

  # Construcción de las coaliciones
  coalitions <- if (lex == FALSE) {
    lapply(1:(2^n - 1), function(S) which(intToBits(S)[1:n] == 1))
  } else {
    unlist(lapply(1:n, function(k) combn(1:n, k, simplify = FALSE)), recursive = FALSE)
  }

  # Construcción del vector de costes
  c <- if (lex == FALSE) v[2^(0:(n-1))] else v[1:n]

  # Se comprueba si la longitud del vector 'v' es un número entero
  if (n %% 1 != 0) {
    stop("'v' must have length 2^n-1")
  }

  # Verificación de que 'lex' es válido
  if (!is.logical(lex) || length(lex) != 1) {
    stop("'lex' must be a single logical value (TRUE or FALSE)")
  }

  # Se comprueba si se verifica la regla v(S) = max(c[S]) para cada coalición
  for (i in seq_along(coalitions)) {
    S <- coalitions[[i]]
    if (length(S) > 1 && v[i] != max(c[S])) {
      stop("'v' is not an airport game")
    }
  }

  # Se devuelve el vector de costes
  return(c)
}
