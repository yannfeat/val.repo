#' Overview of the allocation rules
#'
#' \code{multibasicrules} calculates the contribution vectors resulting from the allocation of payments among different agents, applying various predefined rules.
#' It also generates a graphical representation of the allocations based on the implemented rules.
#'
#' @param c A numeric cost vector.
#' @param rules A character vector specifying the allocation rules. The available rules are:
#' \code{"SFC"}, \code{"SEC"}, \code{"CEC"}, \code{"CP"}, \code{"CEB"}, \code{"SM"}, \code{"CC"}, \code{"SIGMA"} and \code{"PRIOR"}. By default, all the rules
#' are selected.
#' @param draw A logical value indicating whether or not the plot should be generated. By default, \code{draw = TRUE}.
#' @param col A vector that indicates the colors used to represent each agent in the graphical representation. By default,
#' the colors are selected by the function \code{rainbow()}.
#' @param a A numeric value in the range [0,1], controlling the parameterization of the rule. It can only be defined when \code{"SIGMA"} is included in \code{rules}.
#' By default, \code{a = 0.5}.
#' @param order A numeric vector indicating the priority order of agents when making contributions. It can only be defined when \code{"PRIOR"} is included in \code{rules}.
#' By default, agents follow their original indexing and contribute accordingly.
#' @param agents_names A vector defining the name assigned to each agent. By default, the names follow a sequence of natural numbers, starting from 1.
#' @param labels A logical value indicating whether the labels and the title of the plot should be displayed. By default, \code{labels = TRUE}.
#'
#' @return A data frame containing the contribution vectors determined by the selected allocation rules. Additionally, if \code{draw = TRUE},
#' a mosaic plot displaying the allocations obtained for the different rules.
#'
#' @details
#' Let \eqn{X(c)} be the set of cost allocations for \eqn{c \in C^N}.
#' A rule is a mapping \eqn{\mathcal{R}: C^N \rightarrow \mathbb{R}^N} that associates with each problem a contribution vector \eqn{\mathcal{R}_P(c)} such that \eqn{0\leq\mathcal{R}_P(c)\leq c}. In other words, a rule is a mechanism that selects an allocation vector for each airport problem.
#'
#' The various proposed rules, despite their differences, share a key characteristic: for any given problem,
#' each rule selects an allocation vector that belongs to its no-subsidy set.
#' Although these rules have been individually characterized in different functions, the one in question allows for the calculation of all of them at once.
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
#' When \code{"CC"} is included in the \code{rules} argument, the execution time of the function may significantly increase if the number of agents exceeds 150.
#'
#' @examples
#' # All rules with graphical representation
#' c <- c(1, 3, 7, 10) # Cost vector
#' multibasicrules(c)
#'
#' # SEC, CEC and SIGMA rule without plot
#' c <- c(5, 10, 20) # Cost vector
#' multibasicrules(c, rules = c("SEC", "CEC", "SIGMA"), draw = FALSE, a = 0.75,
#' agents_names = c("Alex", "Estela", "Carmen"))
#'
#' @seealso
#' \code{\link{basicrule}}, \code{\link{comparisonallocations}}, \code{\link{plotallocations}}
#'
#' @importFrom graphics mosaicplot mtext
#' @importFrom grDevices rainbow
#'
#' @export
multibasicrules <- function(c, rules = c("SFC", "SEC", "CEC", "CEB", "CP", "SM", "CC", "SIGMA", "PRIOR"),
                       draw = TRUE, col = NULL, a = NULL, order = NULL, agents_names = NULL, labels = TRUE) {

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

  # Verificación de que "rules" ha sido definido como vector
  if (!is.vector(rules) || is.list(rules)) {
    stop("The argument 'rules' must be a vector.")
  }

  # Verificación de que las reglas especificadas son válidas
  invalid_rules <- setdiff(rules, names(rule_))
  if (length(invalid_rules) > 0) {
    stop(paste("Invalid rules specified. The allowed rules to choose from are: 'SFC', 'SEC', 'CEC', 'CEB', 'CP', 'SM', 'CC', 'SIGMA' and 'PRIOR'."))
  }

  # Validaciones específicas para cuando "SIGMA" está dentro de "rules"
  if ("SIGMA" %in% rules) {
    if (is.null(a)) {
      a <- 0.5
    }
    if (!is.numeric(a) || length(a) != 1 || a < 0 || a > 1) {
      stop("The parameter 'a' must be a single numeric value in the range [0,1].")
    }
  } else if (!is.null(a)) {
    warning("The parameter 'a' only applies when the 'SIGMA' rule is selected")
  }

  # Validaciones específicas para cuando "PRIOR" está dentro de "rules"
  if ("PRIOR" %in% rules) {
    if (is.null(order)) {
      order <- seq_len(length(c))
    }
    if (length(order) != length(c) || any(sort(order) != seq_len(length(c)))) {
      stop("The 'order' vector must contain each index from 1 to n exactly once.")
    }
  } else if (!is.null(order)) {
    warning("The argument 'order' only applies when the 'PRIOR' rule is selected")
  }

  # Verificación de que 'c' es un vector numérico
  if (!is.numeric(c)){
    stop("'c' must be a numeric vector")
  }

  # Verificación de que todos los costes sean no negativos (c=>0)
  if (any(c < 0)) {
    stop("'c' must have nonnegative coordinates")
  }

  # Se comprueba que 'draw' es un argumento de tipo lógico
  if (!is.logical(draw) || length(draw) != 1) {
    stop("'draw' must be a single logical value (TRUE or FALSE)")
  }

  # Se comprueba que 'labels' es un argumento de tipo lógico
  if (!is.logical(labels) || length(labels) != 1) {
    stop("'labels' must be a single logical value (TRUE or FALSE)")
  }

  # Si "CC" está dentro de "rules" y n > 150, se lanza un warning:
  if("CC" %in% rules && length(c) > 150) {
    message("Warning: The execution time of the 'CC' rule may be significantly longer. If you want to obtain results more quickly, please do not select 'CC' in the 'rules' vector")
  }

  # Construcción de una lista con todas las reglas solicitadas
  results <- lapply(rules, function(r) {
    if (r == "SIGMA") {
      return(rule_[[r]](c, a))
    } else if (r == "PRIOR") {
      return(rule_[[r]](c, order))
    } else {
      return(suppressMessages(rule_[[r]](c)))
    }
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

    if (labels == TRUE) {
      mosaicplot(results, main = "Rules", col = colors, las = 1, cex.axis = 0.9, ylab = "", border = "grey80")
      mtext("Agents", side = 2, line = 1, font = 2, cex = 1)
      mtext(paste0("c = (", paste(c, collapse = ", "), ")"), side = 1, line = 1, font = 2, cex = 1, adj = 0.5)
    } else{
      mosaicplot(results, col = colors, las = 1, cex.axis = 0.9, ylab = "",main = "", border = "grey80")
    }
  }

  # Se devuelve un data.frame con los resultados
  return(results)
}
