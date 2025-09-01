#' Priority family of rules
#'
#' \code{PRIORrule} calculates the contribution vector selected by a priority rule.
#'
#' @param c A numeric cost vector.
#' @param order A numeric vector indicating the priority order of agents when making contributions.
#' By default, agents follow their original indexing and contribute accordingly.
#'
#' @return A numeric contribution vector, where each element represents the payment of the different agents.
#'
#' @details
#' For each \eqn{c \in C^N} and each \eqn{i \in N}, a priority rule relative to \eqn{\pi \in \Pi^N} is defined by
#' \deqn{
#' \text{P}^{\pi}_i(c)=\text{max}\Big\{0,\ c_i-\text{max}\{c_j:\pi(j)<\pi(i)\}\Big\}.
#' }
#' In this rule, each agent contributes at a different step, so that each one pays all that is necessary until reaching
#' the no-subsidy constraint. Consequently, the agents who arrive first cover the cost of all subsequent agents whose cost is lower.
#'
#' @references
#' Bernárdez Ferradás, A., Mirás Calvo, M. Á., Quinteiro Sandomingo, C., and Sánchez-Rodríguez, E. (2025). Airport problems with cloned agents. [Preprint manuscript].
#'
#' Thomson, W. (2024). Cost allocation and airport problems.
#' \emph{Mathematical Social Sciences}, 31(C), 17–31.
#'
#' @examples
#' # Ascending order
#' c <- c(1, 3, 7, 10) # Cost vector
#' PRIORrule(c)
#'
#' # Fluctuating order
#' c <- c(1, 3, 7, 10) # Cost vector
#' order <- c(2, 1, 4, 3) # Priority order
#' PRIORrule(c, order)
#'
#' @seealso
#' \code{\link{NSfaces}}, \code{\link{basicrule}}, \code{\link{hierarchicalrule}}
#'
#' @export
PRIORrule <- function(c, order = NULL) { # Priority rule relative

  # Verificación de que 'c' es un vector numérico
  if (!is.numeric(c)){
    stop("'c' must be a numeric vector")
  }

  # Verificación de que todos los costes sean no negativos (c=>0)
  if (any(c < 0)) {
    stop("'c' must have nonnegative coordinates")
  }

  # c es el vector de costes de los agentes
  n <- length(c)  # Número de agentes
  x <- numeric(n) # Vector de contribuciones

  # order es el vector que define el orden de prioridad de los agentes (índices)
  # Si no se proporciona ningún orden específico, definimos que por defecto se tome un orden creciente (1,2,...,n)
  if (is.null(order)) {
    order <- 1:n
  }

  # Verificación del orden proporcionado
  if (length(order) != n || any(sort(order) != 1:n)) {
    stop("The 'order' vector must contain each index from 1 to n exactly once")
  }

  # Casuísticas:

  ## Caso 1: Si solo hay un único agente, su contribución es su coste asignado
  if (n == 1) {
    return(c)
  }

  ## Caso 2: Si hay dos agentes o más, se procede de la siguiente manera (resto de casos)
  else{

    # Contribución del primer agente
    x[order[1]] <- c[order[1]]

    # Contribuciones de los agentes restantes
    for (ii in 2:n) {
      x[order[ii]] <- max(0, c[order[ii]] - max(c[order[1:(ii - 1)]]))
    }
  }

  # Se devuelve el vector de contribuciones
  return(x)
}
