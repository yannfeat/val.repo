#' Composition of the no-subsidy set
#'
#' \code{NSstructure} quantifies the key elements of the structure of the no-subsidy set.
#'
#' @param cw A numeric cost vector, with the same length as \code{eta}.
#' @param eta A numeric vector representing the size of each group of cloned agents. All its elements must be positive integers. By default, \code{eta = rep(1, length(cw))}, i.e.,
#' all groups have size 1.
#'
#' @return A list containing the following items:
#'
#' \item{\code{n.faces}}{A positive integer representing the number of faces that form the NS set.}
#'
#' \item{\code{n.full.dim.faces}}{A positive integer indicating the number of full-dimensional faces forming the NS set.}
#'
#' \item{\code{n.extreme.points}}{A positive integer counting the number of extreme points of the NS set.}
#'
#' \item{\code{actual.volume}}{A positive number representing the volume of the NS set.}
#'
#' \item{\code{projected.volume}}{A positive number reflecting the projected volume of the NS set.}
#'
#' @details
#' For any cost vector \eqn{c}, if there are \eqn{n} agents with different cost parameters, the number of faces of the NS set is \eqn{2n-2}.
#' However, the number of full-dimensional faces is indeed affected by the presence of clones. Let \eqn{t\in N}, \eqn{(\eta,c)\in\mathcal{A}^N_t}, and
#' \eqn{\eta\ast c \in C^N}, \eqn{\text{NS}(\eta\ast c)} has \eqn{n+t-2} full-dimensional faces if \eqn{\eta_t=1} and \eqn{n+t-1} full-dimensional faces otherwise.
#' On the other hand, the number of different extreme points of the set \eqn{\text{NS}(\eta\ast c)} is: \eqn{\eta_t \prod_{i \in T \setminus \{t\}} (\eta_i + 1)}
#' (so, when there are no clones, the \eqn{\text{NS}(c)} has \eqn{2^{n-1}} extreme points).
#'
#' Let \eqn{k\in\mathbb{N}} and denote by \eqn{\lambda_k} the \eqn{k}-dimensional Lebesgue measure. If \eqn{X=(X_1,\dots,X_k)} is a random vector
#' with joint density function \eqn{f} and \eqn{\Omega} is a Borel set, then \eqn{P(X\in\Omega)=\int_{\Omega}f(x)d\lambda_k}
#' and the expected value of \eqn{X} is \eqn{\mathbb{E}[X]=\int_{\mathbb{R}^k}x f(x)d\lambda_k}. Given a Borel set \eqn{\Omega\subset\mathbb{R}^k}
#' of positive measure, \eqn{\lambda_k(\Omega)>0}, we say that a random vector \eqn{U=(U_1,\dots,U_k)} has a uniform
#' distribution on \eqn{\Omega}, and we write \eqn{U\thicksim U(\Omega)}, if \eqn{U} has a probability density function \eqn{f(x_1,\dots,x_k)=\frac{1}{\lambda_k(\Omega)}} if
#' \eqn{(x_1,\dots, x_k)\in \Omega} and \eqn{f(x_1,\dots,x_k)=0} otherwise. If \eqn{a=(a_1,\dots,a_k)\in\mathbb{R}^k} with
#' \eqn{0<a_1\leq \dots \leq a_k}, denote
#' \deqn{
#' V_k(a)=\displaystyle\int_0^{a_1}\dots\displaystyle\int_0^{a_k-\sum\limits_{j=1}^{k-1} x_j}d x_k\dots d x_1.
#' }
#' Therefore, for each \eqn{c\in C^N}, the value \eqn{V_{n-1}(c_{-n})} is the
#' \eqn{(n-1)}-Lebesgue measure of \eqn{NS_n(c)}, so \eqn{\lambda_{n-1}(\text{NS}(c))=\sqrt{n}\lambda_{n-1}(\text{NS}_n(c))=\sqrt{n}V_{n-1}(c_{-n})}.
#'
#' @references
#' Bernárdez Ferradás, A., Mirás Calvo, M. Á., Quinteiro Sandomingo, C., and Sánchez-Rodríguez, E. (2025). Airport problems with cloned agents. [Preprint manuscript].
#'
#' González-Díaz, J., Mirás Calvo, M. Á., Quinteiro Sandomingo, C., and Sánchez-Rodríguez, E. (2016). Airport games: the core and its center. \emph{Mathematical Social Sciences}, 82, 105–115.
#'
#' @examples
#' # Without cloned agents
#' c <- c(1, 2, 3, 4)
#' NSstructure(c)
#'
#' # With cloned agents
#' c <- c(1, 2)
#' eta <- c(3, 1)
#' NSstructure(c, eta)
#'
#' @seealso
#' \code{\link{NScheck}}, \code{\link{NSfaces}}, \code{\link{NSset}}, \code{\link{CCrule}}
#'
#' @export
NSstructure <- function(cw, eta = rep(1, length(cw))) {

  # cw: Vector de costes sin agentes clonados (cada respectivo coste solo aparece una vez)
  # eta: Tamaño de cada grupo de agentes clonados

  # Verificación de que 'cw' es un vector numérico
  if (!is.numeric(cw)){
    stop("'cw' must be a numeric vector")
  }

  # Verificación de que 'eta' es un vector numérico
  if (!is.numeric(eta)){
    stop("'eta' must be a numeric vector")
  }

  # Comprobaciones:

  ## Requerimiento 1: Todos los costes del vector cw han de ser no negativos
  if (any(cw < 0)) {
    stop("'cw' must have nonnegative coordinates")
  }

  ## Requerimiento 2: Los componentes del vector eta han de ser números enteros positivos.
  if (any(floor(eta) != eta) || any(eta <= 0)) {
    stop("'eta' must be a vector of positive integers")
  }

  ## Requerimiento 3: Los vectores cw y eta han de tener la misma longitud
  if (length(cw) != length(eta)) {
    stop("'cw' and 'eta' must be the same length")
  }

  ## Aviso: El vector de costes cw no ha de incluir clones
  if (length(cw) != length(unique(cw))) {
    warning("'cw' has cloned agents")
  }

  original.order <- order(cw) # orden original de los costes
  eta <- eta[original.order]  # reordenación de eta en base al orden original de los costes
  eta <- as.numeric(tapply(eta, cw, sum)) # suma de los valores de eta para los costes duplicados
  cw <- sort(unique(cw))    # cw es el vector de costes de los agentes (en orden creciente) sin incluir los clones
  c <- rep(cw,eta) # c es el vector de costes de los agentes (en orden creciente) que incluye todos los costes que puedan estar clonados
  n <- length(c) # Número total de agentes
  t <- length(eta) # Número de costes distintos (es decir, número de conjunto de clones que se pueden formar)

  # Caso n=1:
  if (n==1) {
    stop("'cw' must contain at least two elements")
  }

  # Número de caras
  n.faces <- as.integer(2 * n - 2)

  # Número de caras full-dimensional
  if (eta[t] == 1) {
    n.full.dim.faces <- as.integer(n + t - 2)
  } else {
    n.full.dim.faces <- as.integer(n + t - 1)
  }

  # Número de puntos extremos
  n.extreme.points <- as.numeric(eta[t] * prod(eta[-t] + 1))

  # Volumen proyectado
  projected.volume <- volume(c[-n])

  # Volumen real
  actual.volume <- sqrt(n)*projected.volume

  # Se devuelve la salida como lista
  return(list(n.faces = n.faces, n.full.dim.faces = n.full.dim.faces, n.extreme.points = n.extreme.points,
              actual.volume = actual.volume, projected.volume = projected.volume))
}
