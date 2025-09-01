#' Allocation rule with cloned agents
#'
#' \code{clonesrule} calculates the contribution vector resulting from the payment allocation among the different agents using one of the versions for clones of the various predefined rules.
#'
#' @param cw  A numeric cost vector, with the same length as \code{eta}.
#' @param eta A numeric vector representing the size of each group of cloned agents. All its elements must be positive integers.
#' @param rule A character string specifying the rule to apply. The rules that can be selected are:
#' \code{"SFC"}, \code{"SEC"}, \code{"CEC"}, \code{"CP"}, \code{"CEB"}, \code{"SM"}, \code{"CC"}, \code{"SIGMA"} and \code{"PRIOR"}.
#' @param group_contribution A logical value.
#' By default, if \code{group_contribution = TRUE}, the cost allocation vector stores the aggregated contribution for each group of clones.
#' However, if \code{group_contribution = FALSE}, the cost allocation vector represents the individual contribution of one of the agents in the group of clones.
#' @param a A numeric value in the range [0,1], controlling the parameterization of the rule. It can only be defined when \code{rule = "SIGMA"}.
#' By default, \code{a = 0.5}.
#' @param order A numeric vector indicating the priority order of agents when making contributions. It can only be defined when \code{rule = "PRIOR"}.
#' By default, agents follow their original indexing and contribute accordingly.
#'
#' @return A numeric contribution vector. By default, if \code{group_contribution = TRUE}, each element represents the payment made by each group of cloned agents.
#' However, if \code{group_contribution = FALSE}, each element reflects the individual payment made by a representative agent from each group.
#'
#' @details
#' Let \eqn{\mathcal{R}} be a rule, \eqn{t\in N}, and \eqn{(\eta,c)\in\mathcal{A}^N_t}. For each \eqn{i\in T=\{1,\dots,t\}},
#' the sum of the contributions requested by \eqn{\mathcal{R}} from the group of clones \eqn{N^{\eta}_i} is
#' \eqn{\mathcal{R}(\eta\ast c, N_i^{\eta})=\sum\limits_{j\in N^\eta_i}\mathcal{R}_j(n\ast c)}.
#'
#' The computation of the cost allocation selected by a rule for a given problem can be substantially simplified where there are cloned agents.
#' Through this function, a direct method is proposed to obtain either the aggregate contribution or the individual contribution of each group of cloned agents,
#' based on the associated reduced problem and the number of clones in each group.
#'
#' The version for clones of the SFC, SEC, CEC and CP rules is equal to their respective weighted version, so the formulation of these rules will be the same
#' for the weighted version. Only the CEB rule has a weighted version and a clone version that are different.
#'
#' If a rule \eqn{\mathcal{R}} satisfies equal treatment of equals (cloned agents pay equal amounts), then
#' \eqn{\mathcal{R}(\eta\ast c,N_i^\eta)=\eta_i\mathcal{R}_s(n\ast c)} for any \eqn{s\in N_i^\eta}. All the rules listed, with the
#' exception of the PRIOR rule, satisfy equal treatment of equals. Therefore, the contribution demanded by these rules from a group of clones
#' is divided equally among them.
#'
#' Finally, we define a \eqn{k}-replica of an airport problem as the problem in which every agent is replaced by \eqn{k} clones of itself.
#' If \eqn{k} increases, the number of groups of cloned agents does not change, but the number of agents is large. Thus, we say that a rule satisfies
#' replication invariance if for each \eqn{c\in C^N}, each \eqn{i \in N}, and each \eqn{k\in \mathbb{N}}, we have \eqn{\mathcal{R}(k\ast c,N_i^k)=\mathcal{R}_i(c)};
#' i.e., if in any \eqn{k}-replica of a problem each group of cloned agents contributes an amount independent of \eqn{k}.
#' The SFC, SEC, CEC, CP, and PRIOR rules verify this property, while the others do not.
#'
#' @references
#' Bernárdez Ferradás, A., Mirás Calvo, M. Á., Quinteiro Sandomingo, C., and Sánchez-Rodríguez, E. (2025). Airport problems with cloned agents. [Preprint manuscript].
#'
#' Littlechild, S. C. and Thompson, G. F. (1977). Aircraft landing fees: a game theory approach.
#' \emph{The Bell Journal of Economics}, 8, 186-204.
#'
#' @note
#' When \code{rule = "CC"}, the execution time of the function may significantly increase if the number of individual agents exceeds 150.
#'
#' @examples
#' # Clones SEC rule
#' cw <- c(1, 3, 7, 10) # Different costs
#' eta <- c(3, 4, 1, 2)  # Size of each groups of clones
#' clonesrule(cw, eta, "SEC")
#'
#' # CEC rule satisfies replication invariance
#' cw <- c(1, 5, 12) # Different costs
#' eta <- rep(8, 3) # Size of each groups of clones
#' all.equal(clonesrule(cw, eta, "CEC"), CECrule(cw))
#'
#' @seealso
#' \code{\link{clonesgroups}}, \code{\link{clonesproblem}}, \code{\link{multiclonesrules}}, \code{\link{basicrule}}, \code{\link{weightedrule}}
#'
#' @export
clonesrule <- function(cw, eta, rule, group_contribution = TRUE, a = NULL, order = NULL){ # Rule with clones
  # cw: Vector de costes sin agentes clonados (cada respectivo coste solo aparece una vez)
  # eta: Tamaño de cada grupo de agentes clonados
  # rule selecciona la regla a aplicar: "clonesSFC", "clonesSEC", "clonesCEC", "clonesCEB" "clonesCP", "clonesSM", "clonesSIGMA" O "clonesPRIOR"
  # group_contribution: Si es TRUE, x nos devuelve la asignación agrupada de todos los agentes de cada grupo.
  # group_contribution: Si es FALSE, x nos devuelve la asignación que recibe solo uno de los agentes de cada grupo (se cumple equal treatment of equals, excepto en la PRIOR).
  # a es un parámetro específico de la "clonesSIGMA". Es un valor que oscila entre 0 y 1. Por defecto asignamos a=0.
  # order es un argumento específico de la "clonesPRIOR". Si order = NULL, por defecto definimos que se tome un orden creciente (1,2,...,n)

  # Se estable un identificador para cada una de las reglas correspondientes
  rule_ <- list(
    SFC   = clonesSFCrule,
    SEC   = clonesSECrule,
    CEC   = clonesCECrule,
    CEB   = clonesCEBrule,
    CP    = clonesCPrule,
    SM    = clonesSMrule,
    CC    = clonesCCrule,
    SIGMA = clonesSIGMArule,
    PRIOR = clonesPRIORrule
  )

  # Verificación de que el argumento 'rule' no contiene más de una opción
  if (length(rule) > 1) {
    stop(paste("Invalid rule specified. The argument 'rule' must be one of these: 'SFC', 'SEC', 'CEC', 'CP', 'CEB', 'SM', 'CC', 'SIGMA' or 'PRIOR'"))
  }

  # Verificación de que la regla especificada es válida
  if (!rule %in% names(rule_)) {
    stop(paste("Invalid rule specified. The argument 'rule' must be one of these: 'SFC', 'SEC', 'CEC', 'CP', 'CEB', 'SM', CC', 'SIGMA' or 'PRIOR'"))
  }

  # Validaciones específicas para cuando rule= "SIGMA"
  if (rule == "SIGMA") {
    if (is.null(a)) {
      a <- 0.5
    }
    if (!is.numeric(a) || length(a) != 1) {
      stop("The parameter 'a' must be a single numeric value in the range [0,1]")
    }
    if (a < 0 || a > 1) {
      stop("The parameter 'a' must be a single numeric value in the range [0,1]")
    }
  }

  # Validación específica para el parámetro 'a'
  if (rule != "SIGMA" && !is.null(a)) {
    stop("The parameter 'a' only applies when rule = 'SIGMA'")
  }

  # Validaciones específicas para cuando rule= "PRIOR"
  if (rule == "PRIOR") {
    if (is.null(order)) {
      order <- 1:length(cw)
    }
    if (length(order) != length(cw) || any(sort(order) != 1:length(cw))) {
      stop("The 'order' vector must contain each index from 1 to n exactly once")
    }
  }

  # Validación específica para el argumento 'order'
  if (rule != "PRIOR" && !is.null(order)) {
    stop("The argument 'order' only applies when rule = 'PRIOR'")
  }

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

  # Se comprueba que 'group_contribution' es un argumento de tipo lógico
  if (!is.logical(group_contribution) || length(group_contribution) != 1) {
    stop("'group_contribution' must be a single logical value (TRUE or FALSE)")
  }

  # Se devuelve el vector de contribuciones en base a la regla correspondiente
  if (rule == "SIGMA") {
    x <- suppressWarnings(rule_[[rule]](cw, eta, a))
  } else if (rule == "PRIOR") {
    x <- suppressWarnings(rule_[[rule]](cw, eta, order))
  } else {
    x <- suppressWarnings(rule_[[rule]](cw,eta))
  }

  # Se define la salida en función del argumento 'group_contribution' establecido
  if (group_contribution == TRUE) {
    x <- x
  }
  else {
    x <- x / eta
  }

  return(x)  # Se devuelve el vector de contribuciones
}
