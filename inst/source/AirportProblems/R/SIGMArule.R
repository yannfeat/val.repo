#' Parametric family of rules
#'
#' \code{SIGMArule} calculates the contribution vector selected by a SIGMA rule.
#'
#' @param c A numeric cost vector.
#' @param a A numeric value in the range [0,1], controlling the parameterization of the rule.
#' By default, \code{a = 0.5}.
#'
#' @return A numeric contribution vector, where each element represents the payment of the different agents.
#'
#' @details
#' Let \eqn{N^i_{-}=\{j\in N:j<i\}}. For each \eqn{a \in [0,1]}, each \eqn{c \in C^N}, and each \eqn{i \in N\backslash \{n\}}, a \eqn{\sigma^a} rule is defined by
#' \deqn{
#' \sigma^{a}_i(c)=\text{min}\Bigg\{\text{min}\bigg\{\dfrac{1}{r-(i-1)+a}\Big(c_r-\displaystyle\sum\limits_{j\in N^i_-}\sigma^{a}_{j}(c)\Big):r=i,\dots,n-1\bigg\},
#' \ \dfrac{1}{n+1-i}\Big(c_n-\displaystyle\sum\limits_{j\in N^i_-}\sigma_j^{a}(c)\Big)\Bigg\},}
#' \deqn{\text{and} \ \ \sigma_n^{a}(c)=c_n-\displaystyle\sum\limits_{i=1}^{n-1}\sigma_i^{a}(c).
#' }
#' In this rule, the closer the parameter \eqn{a} is to 0, the more equal the distribution of payments among the agents will be, and vice versa.
#' In fact, it is easy to verify that \eqn{\sigma^0=\text{CEC}\ \ \text{and} \ \ \sigma^1=\text{SM}}.
#'
#' @references
#' Thomson, W. (2024). Cost allocation and airport problems.
#' \emph{Mathematical Social Sciences}, 31(C), 17–31.
#'
#' van Gellekom, J. R. G. and Potters, J. A. M. (1999). Consistent solution rules for standard tree enterprises.
#' Technical Report 9910, University of Nijmegen.
#'
#' @examples
#' c <- c(1, 3, 7, 10) # Cost vector
#' SIGMArule(c) # a=0.5
#'
#' # The SIGMA rule with a=0 is the CEC rule
#' a <- 0
#' all.equal(SIGMArule(c, a), CECrule(c))
#'
#' # The SIGMA rule with a=1 is the SM rule
#' a <- 1
#' all.equal(SIGMArule(c, a), SMrule(c))
#'
#' @seealso
#' \code{\link{CECrule}}, \code{\link{SMrule}}, \code{\link{basicrule}}, \code{\link{hierarchicalrule}}
#'
#' @export
SIGMArule <- function(c, a = 0.5) { # Parametric family of rules (van Gellekom and Potters)

  # Verificación de que 'c' es un vector numérico
  if (!is.numeric(c)){
    stop("'c' must be a numeric vector")
  }

  # Verificación de que todos los costes sean no negativos (c >= 0)
  if (any(c < 0)) {
    stop("'c' must have nonnegative coordinates")
  }

  # a: Parámetro a (0 <= a <= 1). Si a=0, es equivalente a aplicar la CEC rule. Si a=1, es equivalente a aplicar la SM rule.
  # Si no se proporciona ningún parámetro a específico, definimos que por defecto se tome a = 0.5
  # Verificación de que el parámetro "a" sea un único número y esté en el rango [0, 1]
  if (!is.numeric(a) || length(a) != 1) {
    stop("The parameter 'a' must be a single numeric value in the range [0,1]")
  }
  if (a < 0 || a > 1) {
    stop("The parameter 'a' must be a single numeric value in the range [0,1]")
  }

  original.order <- order(c) # Orden original de los costes
  c <- sort(c)    # c es el vector de costes de los agentes (en orden creciente)
  n <- length(c)      # Número de agentes
  x <- numeric(n)     # Vector de contribuciones
  sumas <- 0          # Suma acumulada de las contribuciones

  # Casuísticas:

  ## Caso 1: Si solo hay un único agente, su contribución es su coste asignado
  if (n == 1) {
    return(c)
  }

  ## Caso 2: Si hay dos agentes o más, se procede de la siguiente manera (resto de casos)
  else{

    # Bucle para calcular las contribuciones de cada agente
    for (ii in 1:(n - 1)) {
      # Se calcula la contribución de cada agente i como el mínimo entre dos cantidades
      x[ii] <- min(c((c[ii:(n - 1)] - sumas) / (1 + a):(n - ii + a),
                     (c[n] - sumas) / (n - ii + 1)))

      sumas <- sum(x) # Se suman cada una de las contribuciones obtenidas
    }

    # Contribución del último agente
    x[n] <- c[n] - sumas
  }

  # Se reordenan las contribuciones en base al orden original:
  x <- x[order(original.order)]

  # Se devuelve el vector de contribuciones
  return(x)
}
