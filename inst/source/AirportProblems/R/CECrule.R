#' Constrained equal contributions rule
#'
#' \code{CECrule} calculates the contribution vector selected by the CEC rule.
#'
#' @param c A numeric cost vector.
#'
#' @return A numeric contribution vector, where each element represents the payment of the different agents.
#'
#' @details
#' Let \eqn{N^i_{-}\{j\in N:c_j<c_i\}}. For each \eqn{c\in C^N} and each \eqn{i\in N}, the constrained equal contributions rule is defined by
#' \deqn{
#' \text{CEC}_i(c)=\text{min}\left\{\dfrac{1}{r-i+1}\left(c_r-\sum\limits_{j\in N^i_{-}}\text{CEC}_j(c)\right):r=1,\dots,n\right\}.
#' }
#' This rule offers a different approach to achieving equality.
#' Contributions are distributed as evenly as possible while ensuring compliance with the no-subsidy constraints.
#'
#' The contribution selected by the CEC rule for a problem \eqn{c \in C^N} coincides with the payoff vector assigned
#' by the Dutta-Ray solution (denoted by \eqn{\text{EA}}) to the associated airport game \eqn{v\in G^N}, that is, \eqn{\text{CEC}(c)=\text{EA}(v)}.
#'
#' @references
#' Aadland, D. and Kolpin, V. (1998). Shared irrigation costs: an empirical and axiomatic analysis.
#' \emph{Mathematical Social Sciences}, 35, 203-218.
#'
#' Thomson, W. (2024). Cost allocation and airport problems.
#' \emph{Mathematical Social Sciences}, 31(C), 17–31.
#'
#' @examples
#' c <- c(1, 3, 7, 10) # Cost vector
#' CECrule(c)
#'
#' @seealso
#' \code{\link{SIGMArule}}, \code{\link{basicrule}}, \code{\link{weightedrule}}, \code{\link{clonesrule}}, \code{\link{hierarchicalrule}}
#'
#' @export
CECrule <- function(c) { # Constrained equal contributions rule

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
  n <- length(c)   # Número de agentes
  x <- numeric(n)  # Vector de contribuciones
  sumas <- 0       # Suma acumulada de las contribuciones

  # Bucle para calcular las contribuciones de cada agente
  for (ii in 1:n) {
    # Contribución de cada agente
    x[ii] <- min((c[ii:n] - sumas) / (1:(n - ii + 1)))
    # Suma de contribuciones
    sumas <- sum(x)
  }

  # Se reordenan las contribuciones en base al orden original
  x <- x[order(original.order)]

  # Se devuelve el vector de contribuciones
  return(x)
}
