#' Verification of compliance with the no-subsidy constraints
#'
#' \code{NScheck} evaluates whether or not the no-subsidy constraint is satisfied and, if not,
#' it can also determine one of the coalitions of agents that violates it, as long as the user requests it.
#'
#' @param c A numeric cost vector.
#' @param x A numeric cost allocation vector.
#' @param eta A numeric vector representing the size of each group of cloned agents. All its elements must be positive integers. By default, all components of \code{eta} are set to 1.
#' @param group_contribution A logical value.
#' By default, if \code{group_contribution = TRUE}, \code{x} stores the aggregated contribution for each group of clones.
#' However, if \code{group_contribution = FALSE}, \code{x} represents the individual contribution of one of the agents in the group of clones.
#' @param coalition A logical value. By default, if \code{coalition = FALSE}, the function only returns whether the NS constraint is satisfied or not.
#' However, if \code{coalition = TRUE}, the function also returns the coalition of agents that breach the NS constraint, if it is violated.
#' @param tol Tolerance level for evaluating compliance with the NS constraint.
#'
#' @return If \code{coalition = TRUE}, a logical value (TRUE or FALSE) indicating compliance with the NS constraint.
#'
#' Otherwise, if \code{coalition = FALSE}, a list containing the following items:
#' \tabular{lll}{
#'   \code{flag} \tab \tab  A logical value (TRUE or FALSE) indicating compliance with the NS constraint. \cr
#'   \code{eta} \tab \tab  If the NS constraint is violated, the coalition of agents that breach it will be returned. \cr
#' }
#'
#' @details
#' For each \eqn{c\in C^N} let \eqn{H(c)=\{x\in\mathbb{R}:x(N)=c_n\}} be the hyperplane of \eqn{\mathbb{R}^N}
#' given by all the vectors whose coordinates add up to \eqn{c_n}. A cost allocation for \eqn{c\in C^N} is a vector
#' \eqn{x\in H(c)} such that \eqn{0\leq x\leq c}. The component \eqn{x_i} is the contribution requested from agent \eqn{i}.
#' Let \eqn{X(c)} be the set of cost allocations for \eqn{c\in C^N}. Given \eqn{x\in X(c)}, the difference \eqn{c_i-x_i} is the
#' benefit of agent \eqn{i} at \eqn{x}.
#'
#' A basic requirement is that at an allocation \eqn{x\in X(c)} on group \eqn{N'\subset N}
#' of agents would subsidize the other agents by contributing more than what the group would have to pay on its own. The no-subsidy constraint
#' for the group \eqn{N'\subset N} is \eqn{x(N')\geq \text{max}\{c_j:j\in N'\}}. The set of cost allocations for \eqn{c\in C^N} that satisfy the no-subsidy
#' constraints, the no-subsidy set for short, is given by:
#' \deqn{
#' NS(c)=\{x\in X(c):x(N')\leq\text{max}\{c_j:j\in N'\}, \;\text{for all}\; N'\subset N\}}
#' \deqn{= \{x\in \mathbb{R}^N:x\geq 0, \ x(N)=c_n, \ x_1+\dots+x_i\leq c_i,\;\text{for all}\;i\in N\backslash \{n\}\}
#' }
#' Thus, the no-subsidy correspondence NS assigns to each \eqn{c\in C^N} the set \eqn{NS(c)}.
#'
#' Nevertheless, when a problem has group of cloned agents, the structure of its no-subsidy set is simpler than
#' when all the cost parameters are different. Let \eqn{t\in N}, \eqn{\mathcal{A}_t^N} be the set of pairs \eqn{(\eta,c)\in \mathbb{N}^t\times\mathbb{R}^t} and \eqn{N_s^{\eta}=N_s^{\eta\ast c}=\{j\in N:(n\ast c)_j=c_s\}}.
#' Then the no subsidy set for \eqn{\eta\ast c \in C^N} is:
#' \deqn{
#' NS(\eta\ast c)=\{x\in\mathbb{R}:x\geq 0,\ x(N)=c_t,\ x(N_1^{\eta})+\dots+x(N^{\eta}_s)\leq c_s, \;\text{for all}\; s<t\}.
#' }
#' It is worth noting that all allocation rules proposed in this package satisfy this property.
#'
#' @references
#' Bernárdez Ferradás, A., Mirás Calvo, M. Á., Quinteiro Sandomingo, C., and Sánchez-Rodríguez, E. (2025). Airport problems with cloned agents. [Preprint manuscript].
#'
#' Thomson, W. (2024). Cost allocation and airport problems.
#' \emph{Mathematical Social Sciences}, 31(C), 17–31.
#'
#' @examples
#' # Compliance with the NS constraint
#' c <- c(2, 5, 9)
#' x <- SECrule(c)
#' NScheck(c, x)
#' # Non-compliance with the NS constraint
#' c <- c(2, 3, 7, 10)
#' x <- c(1, 2, 5, 2)
#' NScheck(c, x, coalition = TRUE)
#'
#' @seealso
#' \code{\link{NSfaces}}, \code{\link{NSstructure}}, \code{\link{NSset}}, \code{\link{comparisonallocations}}
#'
#' @export
NScheck <- function(c, x, eta = rep(1, length(x)), group_contribution = TRUE, coalition = FALSE, tol = 1e-6) {

  if (group_contribution == FALSE) {
    x <- x * eta  # Se obtiene la contribución agrupada de cada grupo
  }

  n <- length(c)  # Número de agentes

  # Verificación de que 'x' es un vector numérico
  if (!is.numeric(x)){
    stop("'x' must be a numeric vector")
  }

  # Verificación de que 'c' es un vector numérico
  if (!is.numeric(c)){
    stop("'c' must be a numeric vector")
  }

  # Verificación de que 'eta' es un vector numérico
  if (!is.numeric(eta)){
    stop("'eta' must be a numeric vector")
  }

  # Comprobaciones:

  ## Requerimiento 1: Todas las coordenadas de los vectores x y c han de ser no negativas
  if (any(x < 0) && any(c < 0)) {
    stop("'x' and 'c' must have nonnegative coordinates")
  }
  if (any(x < 0)) {
    stop("'x' must have nonnegative coordinates")
  }
  if (any(c < 0)) {
    stop("'c' must have nonnegative coordinates")
  }

  ## Requerimiento 2: Los vectores x y c han de tener la misma longitud
  if (length(x) != n) {
    stop("'x' and 'c' must have the same length")
  }

  ## Requerimiento 3: La suma de todas las coordenadas de x debe ser igual a la coordenada más alta de c
  sum_x <- sum(x)
  max_c <- max(c)

  # Se comprueba que 'group_contribution' es un argumento de tipo lógico
  if (!is.logical(group_contribution) || length(group_contribution) != 1) {
    stop("'group_contribution' must be a single logical value (TRUE or FALSE)")
  }

  # Se comprueba que 'coalition' es un argumento de tipo lógico
  if (!is.logical(coalition) || length(coalition) != 1) {
    stop("'coalition' must be a single logical value (TRUE or FALSE)")
  }

  # Se revisa si 'tol' es un argumento de tipo numérico
  if (!is.numeric(tol) || length(tol) != 1) {
    stop("'tol' must be a single numeric value")
  }

  # Compara sum(x) con max(c) usando tolerancia
  if (abs(sum_x - max_c) > tol) {
    stop("The sum of the coordinates of 'x' must equal the maximum of 'c'.")
  }

  # Caso especial: Solo hay un único agente
  if (n == 1) {
    return(abs(x - c) < tol)  # Solo se verifica si x es igual a c
  }

  original.order <- order(c)  # orden original de los costes
  c <- c[original.order]  # es el vector de costes de los agentes (en orden creciente)
  x <- x[original.order]  # es el vector de asignaciones de los agentes (en base al orden establecido para c)

  flag <- TRUE  # Si se cumplen todas las restricciones NS, flag=TRUE. En caso contrario, flag=FALSE.
  failing_coalition <- NULL  # Primera coalición donde se produce la violación de la restricción NS (si es que se produce)

  # Verificación de las condiciones de no-subsidio con tolerancia
  for (i in 1:(n-1)) {
    if ((sum(x[1:i]) - c[i]) > tol) {
      flag <- FALSE
      failing_coalition <- sort(original.order[1:i])
      break
    }
  }

  # Devolución de la salida según el valor que tome coalition (TRUE o FALSE).
  if (coalition == TRUE) {
    return(list(flag = flag, failing_coalition = failing_coalition))
  }
  else {
    return(flag)
  }
}
