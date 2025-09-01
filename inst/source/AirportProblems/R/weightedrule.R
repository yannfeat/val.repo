#' Weighted allocation rule
#'
#' \code{weightedrule} calculates the contribution vector resulting from the payment allocation among the different agents using one of the various predefined weighted rules.
#'
#' @param c A numeric cost vector.
#' @param w A numeric weight vector.
#' @param rule A character string specifying the rule to apply.The rules that can be selected are:
#' \code{"SFC"}, \code{"SEC"}, \code{"CSEC"}, \code{"CEC"}, \code{"CP"} and \code{"CEB"}.
#'
#' @return A numeric contribution vector, where each element represents the payment of the different agents.
#'
#' @details
#' Let \eqn{w=(w_i)_{i\in N}\in\mathbb{R}^n} be a positive weight vector, satisfying \eqn{w_i> 0} for all
#' \eqn{i\in N} and \eqn{w(N)=1}. Consider the (\eqn{n-1})-standard simplex, defined as \eqn{\Delta_n=\{x\in\mathbb{R}^n:x\geq 0, \ x_1+\dots+x_n=1\}}.
#' Then, the set of all positive weight vectors corresponds to \eqn{\text{Int}(\Delta_n)}, the interior of the
#' \eqn{(n-1)}-standard simplex.
#'
#' A weighted rule is a mapping \eqn{\mathcal{R}:C^N\times \text{Int}(\Delta_N)\rightarrow \mathbb{R}^N}
#' which associates with a problem \eqn{c\in C^N} and a positive weight vector \eqn{w\in \text{Int}(\Delta_n)} a contribution
#' vector \eqn{\mathcal{R}(c,w)\in X(c)}.
#'
#' It is possible to define weighted versions of the rules: SFC, SEC, CEC, CP and CEB. In fact, two different rules
#' emerge from the standard SEC rule: the weighted SEC rule and the coalition-weighted SEC rule. If \eqn{w_i=w_j} for all \eqn{i,j \in N}, then the solution of weighted SEC\eqn{(c)} and
#' weighted CSEC\eqn{(c)} coincides.
#'
#' In all the rules, the higher the weight \eqn{w_i}, the more the corresponding agent will have to pay, except for the weighted CEB rule
#' (the construction of this rule is based on the concept of allocating 'benefits', so it is logical that it is set up this way).
#' Furthermore, as previously stated, all the rules, except for the weighted CSEC rule, require the weights to be positive.
#' Although the weighted CSEC rule allows zero weights, it requires that at least one weight must be positive.
#'
#' The weighted version of the SFC, SEC, CEC and CP rules is equal to their respective versions for clones, so the formulation of these rules will be the same
#' for the version with clones. Only the CEB rule has a weighted version and a clone version that are different.
#' @references
#' Bernárdez Ferradás, A., Mirás Calvo, M. Á., Quinteiro Sandomingo, C., and Sánchez-Rodríguez, E. (2025a). Airport problems with cloned agents. [Preprint manuscript].
#'
#' Bernárdez Ferradás, A., Mirás Calvo, M. Á., Quinteiro Sandomingo, C., and Sánchez-Rodríguez, E. (2025b). A characterization of the CSEC rule for airport problems. [Prepint manuscript].
#'
#' Sánchez-Rodríguez, E., Mirás Calvo, M. Á., Quinteiro Sandomingo, C., and Núñez Lugilde, I. (2024). Coalition-weighted Shapley values.
#' \emph{International Journal of Game Theory}, 53, 547-577.
#'
#' Thomson, W. (2024). Cost allocation and airport problems.
#' \emph{Mathematical Social Sciences}, 31(C), 17–31.
#'
#' @examples
#' c <- c(1, 3, 3, 7, 10) # Cost vector
#' w <- c(1, 4, 1, 2, 8)  # Weight vector
#'
#' # Weighted SFC rule
#' weightedrule(c, w, "SFC")
#'
#' # Weighted CEB rule
#' weightedrule(c, w, "CEB")
#'
#' # Weighted SEC rule
#' weightedrule(c, w, "SEC")
#'
#' # Weighted CSEC rule
#' w <- c(0, 4, 1, 2, 8) # New weight vector
#' weightedrule(c, w, "CSEC")
#'
#' @seealso
#' \code{\link{NScheck}}, \code{\link{NSset}}, \code{\link{basicrule}}, \code{\link{clonesrule}}
#'
#' @export
weightedrule <- function(c, w, rule) { # Weighted rule
  # c es el vector de costes de los agentes
  # w es el vector de pesos de los agentes
  # rule selecciona la regla a aplicar: "wSFC", "wSEC", "wCEC", "wCP", "wCSEC" o "wCEB"

  # Se estable un identificador para cada una de las reglas correspondientes
  rule_ <- list(
    SFC  = wSFCrule,
    SEC  = wSECrule,
    CEC  = wCECrule,
    CP   = wCPrule,
    CSEC = wCSECrule,
    CEB  = wCEBrule
  )

  # Verificación de que el argumento 'rule' no contiene más de una opción
  if (length(rule) > 1) {
    stop("Invalid rule specified. The argument 'rule' must be only one of these: 'SFC', 'SEC', 'CSEC', 'CEC', 'CEB' or 'CP'")
  }

  # Verificación de que la regla especificada es válida
  if (!rule %in% names(rule_)) {
    stop(paste("Invalid rule specified. The argument 'rule' must be only one of these: 'SFC', 'SEC', 'CEC', 'CP' or 'CSEC'"))
  }

  # Verificación de que 'c' es un vector numérico
  if (!is.numeric(c)){
    stop("'c' must be a numeric vector")
  }

  # Verificación de que 'w' es un vector numérico
  if (!is.numeric(w)){
    stop("'w' must be a numeric vector")
  }

  # Verificación de que todos los costes sean no negativos (c=>0)
  if (any(c < 0)) {
    stop("'c' must have nonnegative coordinates")
  }

  # Verificación de que todos los pesos sean estrictamente positivos (w>0).
  ## Aclaración: La regla CSEC sí que admite pesos nulos, siempre que haya al menos uno positivo.
  if (any(w <= 0) && rule != "CSEC") {
    stop("'w' must have positive coordinates")
  }
  if (all(w == 0) && rule == "CSEC") {
    stop("'w' must have at least one positive coordinate")
  }

  # Verificación de que los vectores c y w tiene la misma longitud
  if (length(c) != length(w)) {
    stop("'c' and 'w' must have the same length")
  }

  # Se devuelve el vector de contribuciones de la regla correspondiente
  return(rule_[[rule]](c, w))
}
