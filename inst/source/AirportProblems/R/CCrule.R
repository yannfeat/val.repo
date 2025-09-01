#' Core-center rule
#'
#' \code{CCrule} calculates the contribution vector selected by the CC rule.
#'
#' @param c A numeric cost vector.
#'
#' @return A numeric contribution vector, where each element represents the payment of the different agents.
#'
#' @details
#' The core-center rule, CC, assigns to each \eqn{c\in C^N} the contribution vector given by the mean value of
#' \eqn{U(c)\thicksim U(\text{NS}(c))}, that is,
#' \deqn{
#' \text{CC}(c)=\mathbb{E}[U(\text{NS}(c))].
#' }
#' Therefore, this rule is the center of gravity of the set of allocations satisfying the no-subsidy constraints. It
#' coincides with the core-center of the cooperative game \eqn{v} associated with \eqn{c\in C^N}.
#'
#' @references
#' González-Díaz, J. and Sánchez-Rodríguez, E. (2007). A natural selection from the core of a TU game: the core-center.
#' \emph{International Journal of Game Theory}, 36, 27-26.
#'
#' González-Díaz, J., Mirás Calvo, M. Á., Quinteiro Sandomingo, C., and Sánchez-Rodríguez (2015).
#' Monotonicity of the core-center of the airport game. \emph{TOP}, 23, 773-798.
#'
#' González-Díaz, J., Mirás Calvo, M. Á., Quinteiro Sandomingo, C., and Sánchez-Rodríguez (2016).
#' Airport games: the core and its center. \emph{Mathematical Social Sciences}, 82, 105-115.
#'
#' @note
#' The execution time of the function may significantly increase if the number of agents exceeds 150.
#'
#' @examples
#' c <- c(1, 3, 7, 10) # Cost vector
#' CCrule(c)
#'
#' @seealso
#' \code{\link{NSset}}, \code{\link{basicrule}}, \code{\link{clonesrule}} \code{\link{hierarchicalrule}}
#'
#' @export
CCrule <- function(c) { # Core-center rule

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
  n <- length(c)   # Número de agentes
  x <- numeric(n)  # Vector de contribuciones

  # Warning para n>150:
  if (n > 150) {
    message("Warning: The execution time may be significantly longer")
  }

  # Casuísticas:

  ## Caso 1: Todos los costes son iguales a 0
  if (cnull == n) {
    x <- numeric(n) # Todos los elementos del vector de contribuciones son 0
  }

  ## Caso 2: Únicamente uno de los costes es positivo
  else if (cnull == n-1) {
    x <- numeric(n)
    x[n] <- c[n]
  }

  ## Caso 3: Dos costes son estrictamente positivos
  else if (cnull == n-2) {
    x <- numeric(n)
    x[n-1] <- c[n-1] / 2
    x[n] <- c[n] - c[n-1] / 2
  }

  ## Caso 4: Todos los costes son iguales
  else if (length(unique(c)) == 1) {
    x <- c / n
  }

  ## Caso 5: Todos los costes son estrictamente positivos
  else if (cnull == 0) {
    V <- volume(c[1:(n-1)])  # Volumen para los primeros n-1 agentes
    for (k in 1:(n - 1)) {
      if ((k + 1) <= (n - 1)) {
        # Si el rango es válido
        A <- c[(k + 1):(n - 1)]
      } else {
        # Si el rango no es válido, generamos un vector vacío
        A <- numeric(0)
      }
      x[k] <- volume(c(c[1:k], c[k], A)) / V
    }
    x[n] <- c[n] - sum(x[1:(n - 1)])
    }

  ## Caso 6: Algunos costes son nulos, pero no todos (al menos 3 no lo son)
  else {
    pos <- c[(cnull + 1):n]  # Costes estrictamente positivos (c > 0)
    xnull <- rep(0, cnull)   # Vector de contribuciones para los agentes con coste nulo
    xpos <- numeric(n - cnull)  # Se inicializa el vector de contribuciones para los agentes con coste positivo

    V <- volume(pos[1:(length(pos) - 1)])  # Volumen inicial sin el último elemento
    for (k in 1:(length(pos) - 1)) { # Se calculan todas las coordenadas excepto la última
      if (k == length(pos) - 1) {
        # Caso especial cuando k es el penúltimo elemento
        xpos[k] <- volume(c(pos[1:k], pos[k])) / V
      } else {
        xpos[k] <- volume(c(pos[1:k], pos[k], pos[(k + 1):(length(pos) - 1)])) / V
      }
    }

    xpos[length(pos)] <- pos[length(pos)] - sum(xpos[1:(length(pos) - 1)])  # Contribución del último agente

    x <- c(xnull, xpos) # Se combinan las contribuciones de los agentes con coste nulo y los de coste positivo
    }

  # Se reordenan las contribuciones en base al orden original
  x <- x[order(original.order)]

  # Se devuelve el vector de contribuciones
  return(x)
}

