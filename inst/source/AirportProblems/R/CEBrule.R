#' Constrained equal benefits rule
#'
#' \code{CEBrule} calculates the contribution vector selected by the CEB rule.
#'
#' @param c A numeric cost vector.
#'
#' @return A numeric contribution vector, where each element represents the payment of the different agents.
#'
#' @details
#' For each \eqn{c\in C^N} and each \eqn{i\in N}, the constrained equal benefits rule is defined by
#' \deqn{
#' \text{CEB}_i(c)=\text{max}\{c_i-\beta,\ 0\}
#' }
#' where \eqn{\beta>0} is chosen so that \eqn{\sum\limits^n_{i=1}\text{CEB}_i(c)=c_n}.
#'
#' This rule focuses on the benefits each agent receives from not having to fully cover their own needs,
#' aiming to distribute them as equitably as possible, without any agent subsidizing another.
#'
#' The contribution selected by the CEB rule for a problem \eqn{c \in C^N} coincides with the payoff vector assigned
#' by the modified nucleolus.
#'
#' @references
#' Hu, C.-C., Tsay, M.-H., and Yeh, C.-H. (2012). Axiomatic and strategic justifications for the
#' constrained equal benefits rule in the airport problem. \emph{Games and Economic Behavior}, 75, 185-197.
#'
#' Potters, J. and Sudhölter, P. (1999). Airport problems and consistent allocation rules.
#' \emph{Mathematical Social Sciences}, 38, 83–102.
#'
#' Sudhölter, P. (1997). The modified nucleolus: Properties and axiomatizations.
#' \emph{International Journal of Game Theory}, 26, 146-182.
#'
#' Thomson, W. (2024). Cost allocation and airport problems.
#' \emph{Mathematical Social Sciences}, 31(C), 17–31.
#'
#' @examples
#' c <- c(1, 3, 7, 10) # Cost vector
#' CEBrule(c)
#'
#' @seealso
#' \code{\link{basicrule}}, \code{\link{weightedrule}}, \code{\link{clonesrule}}, \code{\link{hierarchicalrule}}
#'
#' @export
CEBrule <- function(c) { # Constrained equal benefits rule

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
  cnull <- sum(c==0) # Número de costes que son iguales 0
  n <- length(c) # Número de agentes
  x <- numeric(n)  # Vector de contribuciones

  # Casuísticas:

  ## Caso 1: Todos los costes son iguales a 0
  if (cnull == n) {
    x <- numeric(n) # Todos los elementos del vector de contribuciones son 0
  }

  ## Caso 2: Al menos uno de los costes es estrictamente positivo (resto de casos)
  else{
    control <- FALSE  # Control para el bucle
    ii <- 1  # Índice inicial

    # Hay que encontrar el grupo de agentes con reparto no nulo
    while (!control) {
      cr <- pmax(c - c[ii], numeric(n)) # Vector cr como máximo entre la resta y el vector de ceros
      if (sum(cr) < c[n]) {
        control <- TRUE
      }
      ii <- ii + 1  # Se corrige el índice del primer agente con reparto no nulo
    }

    ii <- ii - 1 # El primer agente con reparto no nulo

    # Con los agentes ii hasta n, se calcula b tal que los beneficios c - x sean constantes e iguales a b
    b <- (sum(c[ii:n]) - c[n]) / (n - ii + 1)

    # Se halla el reparto:
    x <- pmax(c - b, numeric(n)) # Reparto con máxima entre c - b y 0
  }

  # Se reordenan las contribuciones en base al orden original:
  x <- x[order(original.order)]

  # Se devuelve el vector de contribuciones:
  return(x)
}
