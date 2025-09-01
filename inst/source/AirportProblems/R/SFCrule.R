#' Sequential full contributions rule
#'
#' \code{SFCrule} calculates the contribution vector selected by the SFC rule.
#'
#' @param c A numeric cost vector.
#'
#' @return A numeric contribution vector, where each element represents the payment of the different agents.
#'
#' @details
#' For each \eqn{c\in C^N} and each \eqn{i\in N}, let \eqn{N^i_{-}=\{j\in N:c_j<c_i}\} and let \eqn{N^i(c)\subseteq N} be defined by \eqn{N^i(c)=\{j\in N:c_j=c_i\}},
#' the sequential full contribution rule is defined by
#' \deqn{
#' \text{SFC}_i(c)=\frac{c_i-\text{max}\{c_j:j\in N^i_{-}\}}{|N_i(c)|}
#' }
#' According to this rule, an agent does not assist other agents with smaller needs than his own in
#' covering the costs they require, even though he uses the same segments they use (similarly, he does not receive any assistance
#' in covering his own segmental cost from agents with greater needs than his, even though these agents also use his segment).
#' If multiple agents have the same cost parameters, they equally share the cost of the common segment they use.
#'
#' @references
#' Bernárdez Ferradás, A., Mirás Calvo, M. Á., Quinteiro Sandomingo, C., and Sánchez-Rodríguez, E. (2025). Airport problems with cloned agents. [Preprint manuscript].
#'
#' Thomson, W. (2024). Cost allocation and airport problems.
#' \emph{Mathematical Social Sciences}, 31(C), 17–31.
#'
#' @examples
#' c <- c(1, 3, 7, 10) # Cost vector
#' SFCrule(c)
#'
#' @seealso
#' \code{\link{PRIORrule}}, \code{\link{basicrule}}, \code{\link{weightedrule}}, \code{\link{hierarchicalrule}}
#'
#' @importFrom utils tail
#'
#' @export
SFCrule <- function(c) { # Sequential full contributions rule

  # Verificación de que 'c' es un vector numérico
  if (!is.numeric(c)){
    stop("'c' must be a numeric vector")
  }

  # Verificación de que todos los costes sean no negativos (c=>0)
  if (any(c < 0)) {
    stop("'c' must have nonnegative coordinates")
  }

  original.order <- order(c) # orden original de los costes
  c <- sort(c)    # c es el vector de costes de los agentes (en orden creciente)
  n <- length(c)  # Número de agentes
  eta <- c()  # Tamaños de los grupos de agentes con el mismo coste
  cd <- c()   # Vector de costes únicos
  ii <- 1     # Índice inicial

  # Se agrupan los agentes que comparten el mismo coste
  while (ii <= n) {
    J <- which(c == c[ii])  # Encuentra los índices de los agentes con el mismo coste
    eta <- c(eta, tail(J, 1) - sum(eta))  # Tamaño del grupo de agentes con coste c[ii]
    cd <- c(cd, c[ii])  # Agrega el coste único al vector cd
    ii <- tail(J, 1) + 1  # Avanza al siguiente grupo
  }

  # Cálculo de las contribuciones de los grupos
  y <- c(cd[1] / eta[1])  # Contribución del primer grupo

  if (length(cd) > 1) {
    for (ii in 2:length(cd)) {
      y <- c(y, (cd[ii] - cd[ii - 1]) / eta[ii])  # Cálculo recursivo para los demás grupos
    }
  }

  # Se expanden las contribuciones a todos los agentes
  x <- c()
  for (ii in 1:length(cd)) {
    x <- c(x, rep(y[ii], eta[ii]))  # Se replica la contribución para todos los agentes del grupo
  }

  # Se reordenan las contribuciones en base al orden original:
  x <- x[order(original.order)]

  return(x)  # Se devuelve el vector de contribuciones
}
