#' Constrained proportional rule
#'
#' \code{CPrule} calculates the contribution vector selected by the CP rule.
#'
#' @param c A numeric cost vector.
#'
#' @return A numeric contribution vector, where each element represents the payment of the different agents.
#'
#' @details
#' For each \eqn{c\in C^N}, let \eqn{c_0=0} and \eqn{(q_0,q_1,\dots,q_s)\in\mathbb{N}^{s+1}}, with
#' \eqn{0=q_0<q_1<\dots<q_s=n}, defined recursively, for \eqn{j\geq 0}, by
#' \deqn{
#' q_{j+1}=\text{max}\Bigg\{q\in N_{+}^{q_j}:\dfrac{c_q-c_{q_j}}{c_{q_j+1}+\dots+c_q}=\text{min}\bigg\{\dfrac{c_r-c_{q_j}}{c_{q_j+1}+\dots+c_r}:r\in N_{+}^{q_j}\bigg\}\Bigg\}.
#' }
#' Then, for each \eqn{j\in\{0,\dots,s-1\}} and each \eqn{i\in Q_j=\{q_j+1,\dots,q_{j+1}\}},
#' \deqn{
#' \text{CP}_i(c)=\dfrac{c_i}{c(Q_j)}(c_{q_{j+1}}-c_{q_j}).
#' }
#' With this rule, calculating each agent's contribution is not always straightforward.
#' When a coalition of agents violates the NS constraint, it becomes necessary to proceed in two or more steps.
#'
#' The core idea of this rule is proportionality, aiming to ensure that agents' contributions are as close as
#' possible to being proportional to the cost parameters, while respecting these constraints.
#'
#' @references
#' Bernárdez Ferradás, A., Mirás Calvo, M. Á., Quinteiro Sandomingo, C., and Sánchez-Rodríguez, E. (2025). Airport problems with cloned agents. [Preprint manuscript].
#'
#' Thomson, W. (2024). Cost allocation and airport problems.
#' \emph{Mathematical Social Sciences}, 31(C), 17–31.
#'
#' @examples
#' c <- c(1, 3, 7, 10) # Cost vector
#' CPrule(c)
#'
#' @seealso
#' \code{\link{basicrule}}, \code{\link{weightedrule}}, \code{\link{clonesrule}}, \code{\link{hierarchicalrule}}
#'
#' @export
CPrule <- function(c) { # Constrained proportional rule

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
  n <- length(c)        # Número de agentes
  x <- numeric(n)       # Vector de contribuciones

  # Se identifican los agentes con coste cero
  zero_indices <- which(c == 0)

  # Se asignan contribuciones cero a los agentes con coste cero
  if (length(zero_indices) > 0) {
    x[zero_indices] <- 0
  }

  # Se filtran los agentes con coste positivo
  positive_indices <- which(c > 0)
  c_pos <- c[positive_indices]
  n_pos <- length(c_pos)

  if (n_pos > 0) {
    # Variables para el bucle
    J <- 0           # Último índice cubierto
    cJ <- 0          # Coste acumulado asignado

    while (J < n_pos) {
      Int <- (J + 1):n_pos  # Intervalo de agentes sin asignar
      r <- numeric(n_pos)   # Vector de proporciones temporales

      # Cálculo de proporciones para el intervalo
      for (ii in Int) {
        r[ii] <- (c_pos[ii] - cJ) / sum(c_pos[(J + 1):ii])
      }

      # Se determina el índice máximo con la menor proporción
      k <- min(r[Int])
      ii <- max(which(r == k))

      # Se asigna proporcionalmente a los agentes hasta el índice ii
      x[positive_indices[(J + 1):ii]] <- k * c_pos[(J + 1):ii]

      # Se actualizan J y cJ
      J <- ii
      cJ <- sum(x[positive_indices[1:J]])
    }
  }

  # Se reordenan las contribuciones en base al orden original:
  x <- x[order(original.order)]

  return(x) # Se devuelve el vector de contribuciones
}
