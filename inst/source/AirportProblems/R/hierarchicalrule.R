#' Allocation rule according to the agents' hierarchical order
#'
#' \code{hierarchicalrule} calculates the contribution vector resulting from the payment allocation among the different agents using one of the various predefined rules in relation to the agents' hierarchical order.
#'
#' @param c A numeric cost vector.
#' @param P A list showing the agents involved in the different distribution stages.
#' @param rule A character string specifying the rule to apply.The rules that can be selected are:
#' \code{"SFC"}, \code{"SEC"}, \code{"CEC"}, \code{"CP"}, \code{"CEB"}, \code{"SM"}, \code{"CC"} and \code{"SIGMA"}.
#' @param a A numeric value in the range [0,1], controlling the parameterization of the rule. It can only be defined when \code{rule = "SIGMA"}.
#' By default, \code{a = 0.5}.
#'
#' @return A numeric contribution vector, where each element represents the payment of the different agents.
#'
#' @details
#' Let \eqn{N=\{1,\dots,n\}} be a finite set of agents and let \eqn{P=\{P_1,\dots,P_{m+1}\}} be a partition
#' of \eqn{N}, with \eqn{m\leq n-1}. So, the hierarchical structure, \eqn{P_{>}=\{P_1>P_2>\dots>P_m>P_{m+1}\}},
#' implies that agents in \eqn{P_1} have priority over agents in \eqn{N\backslash P_1}, agents in \eqn{P_2} have priority
#' over agents in \eqn{N\backslash P_1\cup P_2}, and so on. Let \eqn{\mathcal{P}(N)} denote the family of all hierarchical structures over \eqn{N}.
#'
#' A hierarchical rule is a mapping \eqn{\mathcal{R}_P:C^N\rightarrow \mathbb{R}^N} that, based on a hierarchical structure \eqn{P_{>}},
#' associates with each problem \eqn{c\in C^N} a cost allocation vector \eqn{\mathcal{R}_P(c)} such that \eqn{0\leq\mathcal{R}_P(c)\leq c}. In other words, the allocation proceeds by assigning costs
#' to agents in higher-hierarchy coalitions before those in lower-hierarchy ones, through the successive application of the rule to the no-subsidy faces of coalitions obtained from \eqn{P_{>}}.
#' These rules are on the boundary of the no-subsidy set.
#'
#' In each stage, the agents share the accumulated costs up to that point. However, the remaining costs are allocated in subsequent stages.
#'
#' @references
#' Bernárdez Ferradás, A., Mirás Calvo, M. Á., Quinteiro Sandomingo, C., and Sánchez-Rodríguez, E. (2025). Airport problems with cloned agents. [Preprint manuscript].
#'
#' Faigle, U. and Kern, W. (1992). The Shapley value for cooperation games under precedence constraints.
#' \emph{International Journal of Game Theory}, 21, 249-266.
#'
#' Fiestras-Janeiro, M. G., Sánchez-Rodríguez, E., and Schuster, M. (2016). A precedence constraint value revisited.
#' \emph{TOP}, 24, 156-179.
#'
#' Mirás Calvo, M. Á., Quinteiro Sandomingo, C., and Sánchez-Rodríguez, E. (2020). The boundary of the core of a balanced game: faces games.
#' \emph{International Journal of Game Theory}, 49(2), 579-599.
#'
#' @examples
#' # Two stages
#' c <- c(1, 3, 7, 10) # Cost vector
#' P <- list(c(1, 2), c(3, 4)) # Agents' hierarchical order
#' hierarchicalrule(c, P, rule = "SEC") # SEC rule
#'
#' # Three stages
#' c <- c(1, 1, 3, 3, 7, 10) # Cost vector
#' P <- list(2, c(1, 3), c(4, 5, 6)) # Agents' hierarchical order
#' hierarchicalrule(c, P, "CEC") # CEC rule
#'
#' @seealso
#' \code{\link{NSfaces}}, \code{\link{PRIORrule}}, \code{\link{basicrule}}
#'
#' @export
hierarchicalrule <- function(c, P, rule, a = NULL) { # Allocation rule according to the established order of priorities
  # c es el vector de costes de los agentes
  # P es la lista que establece el orden de prioridades de las coaliciones a la hora de aplicar la regla
  # rule selecciona la regla a aplicar: "SFC", "SEC", "CEC", "CEB", "CP", "SM" o "CC"
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
    SIGMA = SIGMArule
  )

  # Verificación de que el argumento 'rule' no contiene más de una opción
  if (length(rule) > 1) {
    stop(paste("Invalid rule specified. The argument 'rule' must be only one of these: 'SFC', 'SEC', 'CEC', 'CEB', 'CP', 'SM', 'CC' or 'SIGMA'"))
  }

  # Verificación de que la regla especificada es válida
  if (!rule %in% names(rule_)) {
    stop(paste("Invalid rule specified. The argument 'rule' must be only one of these: 'SFC', 'SEC', 'CEC', 'CP', 'CEB', 'SM', 'CC' or 'SIGMA'"))
  }

  # Se asigna la regla seleccionada
  selected.rule <- rule_[[rule]]

  # Verificación de que P es una lista
  if (!is.list(P)) {
    if (is.vector(P)) {
      P <- list(P)
    } else {
      stop("'P' must be a list of coalitions")
    }
  }

  # Verificación de que 'c' es un vector numérico
  if (!is.numeric(c)){
    stop("'c' must be a numeric vector")
  }

  # Verificación de que todos los costes sean no negativos (c=>0)
  if (any(c < 0)) {
    stop("'c' must have nonnegative coordinates")
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

  N <- seq_along(c) # Conjunto total de agentes
  n <- length(c) # Número de agentes

  # Validación de que P es una lista de coaliciones válidas
  if (any(sapply(P, function(p) any(!p %in% 1:n) || anyDuplicated(p)))) {
    stop("'P' must be a list of coalitions, where each coalition is a subset of agents from 1 to n")
  }

  # Validación de que la unión de todas las coaliciones cubre exactamente a todos los agentes
  if (length(Reduce(union, P)) != n) {
    stop("'P' must cover all agents from 1 to n, so some agents are missing")
  }

  # Validación de que no se dan agentes duplicados a lo largo de las coaliciones en P
  if (length(Reduce(union, P)) != length(Reduce(c, P))) {
    stop("'P' contains duplicate agents across coalitions")
  }


  # Caso especial: P está formada por un único vector de coaliciones => Aplicamos directamente la regla seleccionada sobre el vector de costes
  if (length(P) == 1) {
    if (rule == "SIGMA") {
      return(selected.rule(c, a))
    } else{
      return(selected.rule(c)) # Reparto obtenido para la coalición
    }
  }

  # Contribución para la 1º coalición de agentes
  R <- setdiff(N, P[[1]]) # Coalición complementaria a la dada
  face.game <- NSfaces(c, R) # Aplicación del juego de las caras (todo el pago lo soportan los miembros de la 1º coalición establecida)
  if (rule == "SIGMA") {
    y <- selected.rule(face.game[1,], a)
  } else{
    y <- selected.rule(face.game[1,]) # Reparto obtenido para la 1º coalición
  }

  # Inicializamos la lista de resultados con el obtenido para la primera etapa
  x <- list(y)

  # Contribución para las restantes coaliciones de agentes
  for (i in 2:length(P)) {
    R <- Reduce(intersect, list(setdiff(N, P[[1]]), setdiff(N, P[[i]])))
    face.game <- NSfaces(face.game[2,], R)
    if (rule == "SIGMA") {
      y <- selected.rule(face.game[1,], a)
    } else {
      y <- selected.rule(face.game[1,])
    }
    x[[i]] <- y
  }

  # Sumamos todos los vectores que integran la lista coordenada por coordenada
  x <- Reduce(`+`, x)

  # Se devuelve el vector de contribuciones
  return(x)
}
