#' Allocation rule
#'
#' \code{basicrule} calculates the contribution vector resulting from the payment allocation among the different agents using one of the various predefined rules.
#'
#' @param c A numeric cost vector.
#' @param rule A character string specifying the rule to apply.The rules that can be selected are:
#' \code{"SFC"}, \code{"SEC"}, \code{"CEC"}, \code{"CP"}, \code{"CEB"}, \code{"SM"}, \code{"CC"}, \code{"SIGMA"} and \code{"PRIOR"}.
#' @param a A numeric value in the range [0,1], controlling the parameterization of the rule. It can only be defined when \code{rule = "SIGMA"}.
#' By default, \code{a = 0.5}.
#' @param order A numeric vector indicating the priority order of agents when making contributions. It can only be defined when \code{rule = "PRIOR"}.
#' By default, agents follow their original indexing and contribute accordingly.
#'
#' @return A numeric contribution vector, where each element represents the payment of the different agents.
#'
#' @details
#' A rule is a mapping \eqn{\mathcal{R}:C^N\rightarrow \mathbb{R}^N} which associates with each problem \eqn{c\in C^N} a cost allocation vector
#' \eqn{\mathcal{R}(c)} such that \eqn{0\leq\mathcal{R}(c)\leq c}. In other words, a rule is a mechanism that selects for each airport problem an allocation vector.
#'
#' The various proposed rules, despite their differences, share a key characteristic: for any given problem,
#' each rule selects an allocation vector that belongs to its no-subsidy set.
#' Although these rules have been individually characterized in different functions, the one in question encompasses all of them.
#'
#' @references
#' Bernárdez Ferradás, A., Mirás Calvo, M. Á., Quinteiro Sandomingo, C., and Sánchez-Rodríguez, E. (2025). Airport problems with cloned agents. [Preprint manuscript].
#'
#' Potters, J. and Sudhölter, P. (1999). Airport problems and consistent allocation rules.
#' \emph{Mathematical Social Sciences}, 38, 83–102.
#'
#' Thomson, W. (2024). Cost allocation and airport problems.
#' \emph{Mathematical Social Sciences}, 31(C), 17–31.
#'
#' @note
#' When \code{rule = "CC"}, the execution time of the function may significantly increase if the number of agents exceeds 150.
#'
#' @examples
#' c <- c(1, 3, 7, 10) # Cost vector
#'
#' # SEC rule
#' basicrule(c, rule = "SEC")
#'
#' # PRIOR rule
#' order <- c(1, 3, 2, 4)
#' basicrule(c, "PRIOR", order = order)
#'
#' @seealso
#' \code{\link{NSset}}, \code{\link{weightedrule}}, \code{\link{clonesrule}}, \code{\link{hierarchicalrule}}, \code{\link{multibasicrules}}
#'
#' @export
basicrule <- function(c, rule, a = NULL, order = NULL){ # Standard rule
  # c: Vector de costes de los agentes
  # rule selecciona la regla a aplicar: "SFC", "SEC", "CEC", "CEB" "CP", "SM", "CC", SIGMA" o "PRIOR"
  # a es un parámetro específico de la "SIGMA". Es un valor que oscila entre 0 y 1. Por defecto asignamos a=0.
  # order es un argumento específico de la "PRIOR". Si order = NULL, por defecto definimos que se tome un orden creciente (1,2,...,n)

  # Se estable un identificador para cada una de las reglas correspondientes
  rule_ <- list(
    SFC   = SFCrule,
    SEC   = SECrule,
    CEC   = CECrule,
    CEB   = CEBrule,
    CP    = CPrule,
    SM    = SMrule,
    CC    = CCrule,
    SIGMA = SIGMArule,
    PRIOR = PRIORrule
  )

  # Verificación de que el argumento 'rule' no contiene más de una opción
  if (length(rule) > 1) {
    stop(paste("Invalid rule specified. The argument 'rule' must be only one of these: 'SFC', 'SEC', 'CEC', 'CP', 'CEB', 'SM', 'CC', 'SIGMA' or 'PRIOR'"))
  }

  # Verificación de que la regla especificada es válida
  if (!rule %in% names(rule_)) {
    stop(paste("Invalid rule specified. The argument 'rule' must be only one of these: 'SFC', 'SEC', 'CEC', 'CP', 'CEB', 'SM', 'CC', 'SIGMA' or 'PRIOR'"))
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
    warning("The parameter 'a' only applies when rule = 'SIGMA'")
  }

  # Validaciones específicas para cuando rule= "PRIOR"
  if (rule == "PRIOR") {
    if (is.null(order)) {
      order <- 1:length(c)
    }
    if (length(order) != length(c) || any(sort(order) != 1:length(c))) {
      stop("The 'order' vector must contain each index from 1 to n exactly once")
    }
  }

  # Validación específica para el argumento 'order'
  if (rule != "PRIOR" && !is.null(order)) {
    warning("The argument 'order' only applies when rule = 'PRIOR'")
  }

  # Verificación de que 'c' es un vector numérico
  if (!is.numeric(c)){
    stop("'c' must be a numeric vector")
  }

  # Verificación de que todos los costes sean no negativos (c=>0)
  if (any(c < 0)) {
    stop("'c' must have nonnegative coordinates")
  }

  # Se devuelve el vector de contribuciones en base a la regla correspondiente
  if (rule == "SIGMA") {
    return(rule_[[rule]](c, a))
  } else if (rule == "PRIOR") {
    return(rule_[[rule]](c, order))
  } else {
    return(rule_[[rule]](c))
  }
}
