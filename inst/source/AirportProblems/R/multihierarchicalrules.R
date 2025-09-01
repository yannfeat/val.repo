#' Overview of the allocation rules according to the agents' hierarchical order
#'
#' \code{multihierarchicalrules} calculates the contribution vectors resulting from the allocation of payments among different agents, applying various predefined rules in relation to the agents' hierarchical order.
#' It also generates a graphical representation of the allocations based on the implemented rules.
#'
#' @param c A numeric cost vector.
#' @param P A list showing the agents involved in the different distribution stages.
#' @param rules A character vector specifying the allocation rules. The available rules are:
#' \code{"SFC"}, \code{"SEC"}, \code{"CEC"}, \code{"CP"}, \code{"CEB"}, \code{"SM"}, \code{"CC"} and \code{"SIGMA"}. By default, all the rules
#' are selected.
#' @param draw A logical value indicating whether the plot should be generated. By default, \code{draw = TRUE}.
#' @param col A vector that indicates the colors used to represent each agent in the graphical representation. By default,
#' the colors are selected by the function \code{rainbow()}.
#' @param a A numeric value in the range [0,1], controlling the parameterization of the rule. It can only be defined when \code{"SIGMA"} is included in \code{rules}.
#' By default, \code{a = 0.5}.
#' @param agents_names A vector defining the name assigned to each agent. By default, the names follow a sequence of natural numbers, starting from 1.
#' @param labels A logical value indicating whether the labels and the title of the plot should be displayed. By default, \code{labels = TRUE}.
#'
#' @return A data frame containing the contribution vectors determined by the selected allocation rules. Additionally, if \code{draw = TRUE},
#' a mosaic plot displaying the allocations obtained for the different rules.
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
#' # All rules in two stages with graphical representation
#' c <- c(1, 3, 3, 7, 10) # Cost vector
#' P <- list(c(1, 2, 3), c(4, 5)) # Agents' hierarchical order
#' multihierarchicalrules(c, P)
#'
#' # SEC, CEC and SM rule in three stages without plot
#' c <- c(5, 10, 20, 20, 30, 50) # Cost vector
#' P <- list(c(1, 2), c(3, 4), c(5, 6)) # Agents' hierarchical order
#' multihierarchicalrules(c, P, rules = c("SEC", "CEC", "SM"), draw = FALSE,
#' agents_names = c("Alex", "Estela", "Carmen", "Miguel", "Gloria", "Brais"))
#'
#' @seealso
#' \code{\link{NSfaces}}, \code{\link{PRIORrule}}, \code{\link{basicrule}}, \code{\link{comparisonallocations}}, \code{\link{plotallocations}}
#'
#' @importFrom graphics mosaicplot mtext
#' @importFrom grDevices rainbow
#'
#' @export
multihierarchicalrules <- function(c, P, rules = c("SFC", "SEC", "CEC", "CEB", "CP", "SM", "CC", "SIGMA"),
                               a = NULL, draw = TRUE, col = NULL, agents_names = NULL, labels = TRUE) {

  # Se establece un identificador para cada una de las reglas correspondientes
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

  # Verificación de que "rules" es un vector válido
  if (!is.vector(rules) || is.list(rules)) {
    stop("The argument 'rules' must be a vector.")
  }

  # Verificación de que las reglas especificadas son válidas
  invalid_rules <- setdiff(rules, names(rule_))
  if (length(invalid_rules) > 0) {
    stop(paste("Invalid rules specified. The allowed rules to choose from are: 'SFC', 'SEC', 'CEC', 'CEB', 'CP', 'SM', 'CC' and 'SIGMA'."))
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

  # Validación de que "P" es una lista de coaliciones válida
  if (!is.list(P)) {
    if (is.vector(P)) {
      P <- list(P)
    } else {
      stop("'P' must be a list of coalitions.")
    }
  }

  # Verificación de que todos los agentes están cubiertos por las coaliciones en P
  n <- length(c)
  if (length(Reduce(union, P)) != n) {
    stop("'P' must cover all agents from 1 to n, so some agents are missing.")
  }
  if (length(Reduce(union, P)) != length(Reduce(c, P))) {
    stop("'P' contains duplicate agents across coalitions.")
  }

  # Verificación de que 'c' es un vector numérico
  if (!is.numeric(c)){
    stop("'c' must be a numeric vector")
  }

  # Verificación de que todos los costes sean no negativos (c >= 0)
  if (any(c < 0)) {
    stop("'c' must have nonnegative coordinates.")
  }

  # Se comprueba que 'draw' es un argumento de tipo lógico
  if (!is.logical(draw) || length(draw) != 1) {
    stop("'draw' must be a single logical value (TRUE or FALSE)")
  }

  # Se comprueba que 'labels' es un argumento de tipo lógico
  if (!is.logical(labels) || length(labels) != 1) {
    stop("'labels' must be a single logical value (TRUE or FALSE)")
  }

  # Cálculo de resultados para cada regla
  results <- lapply(rules, function(rule) {
    if (rule == "SIGMA") {
      hierarchicalrule(c, P, rule, a)
    } else {
      hierarchicalrule(c, P, rule)
    }
  })

  # Transformación de los resultados en un data.frame
  results <- as.data.frame(do.call(rbind, results))
  rownames(results) <- rules # Asignación de nombres a las filas según las reglas
  # Verificación y asignación de nombres personalizados a los agentes
  if (is.null(agents_names)) {
    colnames(results) <- paste0(seq_along(c)) # Nombres por defecto: 1:n
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
      mosaicplot(results, main = "Priority rules", col = colors, las = 1, cex.axis = 0.9, ylab = "", border = "grey80")
      mtext("Agents", side = 2, line = 1, font = 2, cex = 1)
      P_text <- paste(sapply(P, function(x) paste0("{", paste(x, collapse = ", "), "}")), collapse = ", ")
      mtext(paste0("c = (", paste(c, collapse = ", "), ")  with  P = (", P_text, ")"), side = 1, line = 1, font = 2, cex = 1, adj = 0.5)
    } else {
      mosaicplot(results, col = colors, las = 1, cex.axis = 0.9, ylab = "", main = "", border = "grey80")
    }
  }

  # Retorno de un data.frame con los resultados
  return(results)
}
