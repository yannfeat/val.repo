#' Slack maximizer rule
#'
#' \code{SMrule} calculates the contribution vector selected by the SM rule.
#'
#' @param c A numeric cost vector.
#'
#' @return A numeric contribution vector, where each element represents the payment of the different agents.
#'
#' @details
#' For each \eqn{c\in C^N} and each \eqn{i\in N\backslash\{n\}}, the slack maximizer rule is defined by
#' \deqn{
#' \text{SM}_i(c) = \text{min} \Bigg\{ \dfrac{1}{r-i+2} \Big( c_r - \displaystyle\sum\limits_{j \in N^i_{-}} \text{SM}_j(c) \Big):r=i,\dots,n-1 \Bigg\},\
#' \text{SM}_n(c)=c_n-\displaystyle\sum\limits^{n-1}_{i=1}SM_i(c)
#' }
#' This rule aims to maximize the 'slacks',
#' that is, the available margin for each agent within the imposed constraints.
#'
#' The contribution selected by the SM rule for a problem \eqn{c \in C^N} coincides with the payoff vector assigned
#' by the nucleolus to the associated cost game \eqn{v\in G^N}, that is, \eqn{\text{SM}(c)=\text{Nu}(v)}.
#'
#' @references
#' Littlechild, S. C. (1974). A simple expression for the nucleolus in a special case. \emph{International Journal of Game Theory}, 3(1), 21-29.
#'
#' Thomson, W. (2024). Cost allocation and airport problems.
#' \emph{Mathematical Social Sciences}, 31(C), 17–31.
#'
#' @examples
#' c <- c(1, 3, 7, 10) # Cost vector
#' SMrule(c)
#'
#' @seealso
#' \code{\link{SIGMArule}}, \code{\link{basicrule}}, \code{\link{hierarchicalrule}}
#'
#' @export
SMrule <- function(c) { # Slack maximizer rule

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
  x <- numeric(n) # Vector de contribuciones
  sumas <- 0      # Suma acumulada de contribuciones

  # Casuísticas:

  ## Caso 1: Si solo hay un único agente, su contribución es su coste asignado
  if (n == 1) {
    return(c)
  }

  ## Caso 2: Si hay dos agentes o más, se procede de la siguiente manera (resto de casos)
  else{
    for (ii in 1:(n - 1)) {
      # Contribución de cada agente
      x[ii] <- min((c[ii:(n - 1)] - sumas) / (2:(n - ii + 1)))
      # Suma de contribuciones
      sumas <- sum(x)
    }

    # Contribución del último agente
    x[n] <- c[n] - sumas
  }

  # Se reordenan las contribuciones en base al orden original:
  x <- x[order(original.order)]

  return(x)  # Se devuelve el vector de contribuciones
}
