#' Overview of the weighted allocation rules
#'
#' \code{multiweightedrules} calculates the contribution vectors resulting from the allocation of payments among different agents, applying various predefined weighted rules.
#' It also generates a graphical representation of the allocations based on the implemented weighted rules.
#'
#' @param c A numeric cost vector.
#' @param w A numeric weight vector.
#' @param rules A character vector specifying the allocation rules. The available rules are:
#' \code{"SFC"}, \code{"SEC"}, \code{"CSEC"}, \code{"CEC"}, \code{"CP"} and \code{"CEB"}. By default, all the rules
#' are selected.
#' @param draw A logical value indicating whether the plot should be generated. By default, \code{draw = TRUE}.
#' @param col A vector that indicates the colors used to represent each agent in the graphical representation. By default,
#' the colors are selected by the function \code{rainbow()}.
#' @param agents_names A vector defining the name assigned to each agent. By default, the names follow a sequence of natural numbers, starting from 1.
#' @param labels A logical value indicating whether the labels and the title of the plot should be displayed. By default, \code{labels = TRUE}.
#'
#' @return A data frame containing the contribution vectors determined by the selected allocation rules. Additionally, if \code{draw = TRUE},
#' a mosaic plot displaying the allocations obtained for the different weighted rules.
#'
#' @details
#' Let \eqn{w=(w_i)_{i\in N}\in\mathbb{R}^n} be a positive weight vector, satisfying \eqn{w_i> 0} for all
#' \eqn{i\in N} and \eqn{w(N)=1}. Consider the \eqn{(n-1)}-standard simplex, defined as \eqn{\Delta_n=\{x\in\mathbb{R}^n:x\geq 0, \ x_1+\dots+x_n=1\}}.
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
#' Furthermore, as previously stated, all the rules, except for the weighted CSEC rule, require the weights to be positive. However, to standardize the criterion,
#' we establish that the CSEC rule also demands positive weights.
#'
#' The weighted version of the SFC, SEC, CEC and CP rules is equal to their respective versions for clones, so the formulation of these rules will be the same
#' for the version with clones. Only the CEB rule has a weighted version and a clone version that are different.
#'
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
#' # All weighted rules with graphical representation
#' c <- c(1, 3, 3, 7, 10) # Cost vector
#' w <- c(1, 4, 1, 2, 8) # Weight vector
#' multiweightedrules(c, w)
#'
#' # Weighted SEC, CEC and CP rule without plot
#' c <- c(5, 10, 20)
#' w <- c(3, 2, 1)
#' multiweightedrules(c, w, rules = c("SEC", "CEC", "CP"), draw = FALSE,
#' agents_names = c("Alex", "Estela", "Carmen"))
#'
#' @seealso
#' \code{\link{weightedrule}}, \code{\link{comparisonallocations}}, \code{\link{plotallocations}}
#'
#' @importFrom graphics mosaicplot mtext
#' @importFrom grDevices rainbow
#'
#' @export
multiweightedrules <- function(c, w, rules = c("SFC", "SEC", "CSEC", "CEC", "CEB", "CP"),
                               draw = TRUE, col = NULL, agents_names = NULL, labels = TRUE) {

  # Se estable un identificador para cada una de las reglas correspondientes
  rule_ <- list(
    SFC  = wSFCrule,
    SEC  = wSECrule,
    CEC  = wCECrule,
    CEB  = wCEBrule,
    CP   = wCPrule,
    CSEC = wCSECrule
  )

  # Verificación de que "rules" ha sido definido como vector
  if (!is.vector(rules) || is.list(rules)) {
    stop("The argument 'rules' must be a vector.")
  }

  # Verificación de que las reglas especificadas son válidas
  invalid_rules <- setdiff(rules, names(rule_))
  if (length(invalid_rules) > 0) {
    stop(paste("Invalid rules specified. The allowed rules to choose from are: 'SFC', 'SEC', 'CSEC', 'CEC', 'CEB' and 'CP'."))
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
  ## Aclaración: La regla CSEC sí que admitiría pesos positivos, pero dado que el resto no, no lo permitimos para ninguna.
  if (any(w <= 0)) {
    stop("'w' must have positive coordinates")
  }

  # Verificación de que los vectores c y w tiene la misma longitud
  if (length(c) != length(w)) {
    stop("'c' and 'w' must have the same length")
  }

  # Se comprueba que 'draw' es un argumento de tipo lógico
  if (!is.logical(draw) || length(draw) != 1) {
    stop("'draw' must be a single logical value (TRUE or FALSE)")
  }

  # Se comprueba que 'labels' es un argumento de tipo lógico
  if (!is.logical(labels) || length(labels) != 1) {
    stop("'labels' must be a single logical value (TRUE or FALSE)")
  }

  # Construcción de una lista con todas las reglas solicitadas
  results <- lapply(rules, function(r) {
    return(rule_[[r]](c, w))
  })

  # Transformación de los resultados obtenidos en un data.frame
  results <- as.data.frame(do.call(rbind, results))
  rownames(results) <- rules  # Nombre de las filas en base a las reglas estblecidas
  # Verificación y asignación de nombres personalizados a los agentes
  if (is.null(agents_names)) {
    colnames(results) <- paste0(seq_along(c))  # Nombres por defecto: 1:n
  } else {
    if (length(agents_names) != length(c)) {
      stop("'agent_names' and 'c' must have the same length.")
    }
    colnames(results) <- agents_names
  }

  # Construcción de un gráfico de mosaico en base a los valores dados
  if (draw == TRUE){
    num_colors <- ncol(results)
    if (is.null(col)) {
      colors <- rainbow(num_colors)
    } else {
      invalid_colors <- c()
      for (i in 1:length(col)) {
        is_valid_color <- tryCatch({col2rgb(col[i]); TRUE}, error = function(e) {FALSE})
        if (!is_valid_color) {
          invalid_colors <- c(invalid_colors, col[i])
        }
      }
      if (length(invalid_colors) > 0) {
        stop(paste("The following colors are not valid: ", paste(invalid_colors, collapse = ", "), sep = ""))
      }
      colors = rep(col, length.out = num_colors)
    }

    if (labels == TRUE){
      mosaicplot(results, main = "Weighted rules", col = colors, las = 1, cex.axis = 0.9, ylab = "",border = "grey80")
      mtext("Agents", side = 2, line = 1, font = 2, cex = 1)
      mtext(paste0("c = (", paste(c, collapse = ", "), ")  with  w = (", paste(w, collapse = ", "), ")"),
            side = 1, line = 1, font = 2, cex = 1)}
    else {
      mosaicplot(results, col = colors, las = 1, cex.axis = 0.9, ylab = "",main = "",border = "grey80")
    }
  }

  # Se devuelve un data.frame con los resultados
  return(results)
}
