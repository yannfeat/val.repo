#' Sequential equal contributions rule
#'
#' \code{SECrule} calculates the contribution vector selected by the SEC rule.
#'
#' @param c A numeric cost vector.
#'
#' @return A numeric contribution vector, where each element represents the payment of the different agents.
#'
#' @details
#' For each \eqn{c\in C^N} and each \eqn{i\in N}, the sequential equal contributions rule is defined by
#' \deqn{
#' \text{SEC}_i=\frac{c_1}{n}+\frac{c_2-c_1}{n-1}+\dots+\frac{c_i-c_{i-1}}{n-i+1}
#' }
#' This rule is based on applying an equal division to each segment separately,
#' so that all agents using a given segment contribute equally to its cost. Each agent's contribution is then obtained as a sum of terms,
#' one for each of the segments they use.
#'
#' The contribution selected by the SEC rule for a problem \eqn{c \in C^N} coincides with the payoff vector assigned
#' by the Shapley value to the associated cost game \eqn{v\in G^N}, that is, \eqn{\text{SEC}(c)=\text{Sh}(v)}.
#'
#' @references
#' Chun, Y., Hu, C.-C., and Yeh, C. (2012). Characterizations of the sequential equal contributions rule for the
#' airport problem. \emph{International Journal of Economic Theory}, 8, 77-85.
#'
#' Littlechild, S.C. and Owen, G. (1973). A simple expression for the Shapley value in a special case.
#' \emph{Management Science}, 20, 370-372.
#'
#' Thomson, W. (2024). Cost allocation and airport problems.
#' \emph{Mathematical Social Sciences}, 31(C), 17–31.
#'
#' @examples
#' c <- c(1, 3, 7, 10) # Cost vector
#' SECrule(c)
#'
#' @seealso
#' \code{\link{basicrule}}, \code{\link{weightedrule}}, \code{\link{clonesrule}}, \code{\link{hierarchicalrule}}
#'
#' @export
SECrule <- function(c){ # Sequential equal contributions rule

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
  x <- numeric(n)  # Vector de contribuciones

  # Casuísticas:

  ## Caso 1: Si solo hay un único agente, su contribución es su coste asignado
  if (n == 1) {
    return(c)
  }

  ## Caso 2: Si hay dos agentes o más, se procede de la siguiente manera (resto de casos)
  else{
    x[1] <- c[1] / n  # Contribución del primer agente

    # Los demás valores se calculan por recurrencia
    for (ii in 2:n) {
      x[ii] <- x[ii-1] + (c[ii] - c[ii-1]) / (n - ii + 1)
    }
  }

  # Se reordenan las contribuciones en base al orden original
  x <- x[order(original.order)]

  # Se devuelve el vector de contribuciones
  return(x)
}
