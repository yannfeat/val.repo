#' Overview of the allocation rules with cloned agents
#'
#' \code{multiclonesrules} calculates the contribution vectors resulting from the allocation of payments among different agents, applying the versions for clones of various predefined rules.
#' It also generates a graphical representation of the allocations based on the implemented rules.
#'
#' @param cw A numeric cost vector, with the same length as \code{eta}.
#' @param eta A numeric vector representing the size of each group of cloned agents. All its elements must be positive integers.
#' @param rules A character vector specifying the allocation rules. The available rules are:
#' \code{"SFC"}, \code{"SEC"}, \code{"CEC"}, \code{"CP"}, \code{"CEB"}, \code{"SM"}, \code{"CC"}, \code{"SIGMA"} and \code{"PRIOR"}. By default, all the rules
#' are selected.
#' @param group_contribution A logical value.
#' By default, if \code{group_contribution = TRUE}, the cost allocation vector stores the aggregated contribution for each group of clones.
#' However, if \code{group_contribution = FALSE}, the cost allocation vector represents the individual contribution of one of the agents in the group of clones.
#' @param draw A logical value indicating whether the plot should be generated. By default, \code{draw = TRUE}.
#' @param col A vector that indicates the colors used to represent each agent in the graphical representation. By default,
#' the colors are selected by the function \code{rainbow()}.
#' @param a A numeric value in the range [0,1], controlling the parameterization of the rule. It can only be defined when \code{"SIGMA"} is included in \code{rules}.
#' By default, \code{a = 0.5}.
#' @param order A numeric vector indicating the priority order of agents when making contributions. It can only be defined when \code{"PRIOR"} is included in \code{rules}.
#' By default, agents follow their original indexing and contribute accordingly.
#' @param agents_names A vector defining the name assigned to each group of clones. By default, the names follow a sequence of natural numbers, starting from 1.
#' @param labels A logical value indicating whether the labels and the title of the plot should be displayed. By default, \code{labels = TRUE}.
#'
#' @return A data frame containing the contribution vectors determined by the selected allocation rules. Additionally, if \code{draw = TRUE},
#' a mosaic plot displaying the allocations obtained for the different rules.
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
#' When \code{"CC"} is included in the \code{rules} argument, the execution time of the function may significantly increase if the number of individual agents exceeds 150.
#'
#' @examples
#' # All rules for clones with graphical representation
#' cw <- c(1, 3, 7, 10) # Different costs
#' eta <- c(3, 4, 1, 2)  # Size of each groups of clones
#' multiclonesrules(cw, eta)
#'
#' # SEC, CEC and CP rule for clones without plot
#' cw <- c(5, 10, 20) # Different costs
#' eta <- c(8, 2, 4) # Size of each groups of clones
#' multiclonesrules(cw, eta, rules = c("SEC", "CEC", "CP"),
#' group_contribution = FALSE, draw = FALSE,
#' agents_names = c("Suppliers", "Wholesalers", "Retailers"))
#'
#' @seealso
#' \code{\link{clonesgroups}}, \code{\link{clonesproblem}}, \code{\link{clonesrule}}, \code{\link{comparisonallocations}}, \code{\link{plotallocations}}
#'
#' @importFrom graphics mosaicplot mtext
#' @importFrom grDevices rainbow
#'
#' @export
multiclonesrules <- function(cw, eta, rules = c("SFC", "SEC", "CEC", "CEB", "CP", "SM", "CC", "SIGMA", "PRIOR"),
                             group_contribution = TRUE, draw = TRUE, col = NULL, a = NULL, order = NULL, agents_names = NULL, labels = TRUE) {

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
      order <- seq_len(length(cw))
    }
    if (length(order) != length(cw) || any(sort(order) != seq_len(length(cw)))) {
      stop("The 'order' vector must contain each index from 1 to n exactly once.")
    }
  } else if (!is.null(order)) {
    warning("The argument 'order' only applies when the 'PRIOR' rule is selected")
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

  # Se comprueba que 'draw' es un argumento de tipo lógico
  if (!is.logical(draw) || length(draw) != 1) {
    stop("'draw' must be a single logical value (TRUE or FALSE)")
  }

  # Se comprueba que 'labels' es un argumento de tipo lógico
  if (!is.logical(labels) || length(labels) != 1) {
    stop("'labels' must be a single logical value (TRUE or FALSE)")
  }

  # Si "CC" está dentro de "rules" y n > 150, se lanza un warning:
  if("CC" %in% rules && length(rep(cw, eta)) > 150) {
    message("Warning: The execution time of the 'CC' rule may be significantly longer. If you want to obtain results more quickly, please do not select 'CC' in the 'rules' vector")
  }

  # Construcción de una lista con todas las reglas solicitadas
  results <- lapply(rules, function(r) {
    if (r == "SIGMA") {
      return(suppressWarnings(rule_[[r]](cw, eta, a)))
    } else if (r == "PRIOR") {
      return(suppressWarnings(rule_[[r]](cw, eta, order)))
    } else {
      return(suppressMessages(suppressWarnings(rule_[[r]](cw, eta))))
    }
  })

  # Transformación de los resultados obtenidos en un data.frame
  results <- as.data.frame(do.call(rbind, results))
  rownames(results) <- rules  # Nombre de las filas en base a las reglas estblecidas
  # Verificación y asignación de nombres personalizados a los agentes
  if (is.null(agents_names)) {
    colnames(results) <- paste0(seq_along(cw))  # Nombres por defecto: 1:n
  } else {
    if (length(agents_names) != length(cw)) {
      stop("'agent_names' and 'cw' must have the same length.")
    }
    colnames(results) <- agents_names
  }

  # Modificación para dividir cada regla por eta si 'group_contribution = FALSE'
  if (!group_contribution) {
    results <- as.data.frame(t(apply(results, 1, function(x) x / eta)))
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
      mosaicplot(results, main = "Clones rules", col = colors, las = 1, cex.axis = 0.9, ylab = "",border = "grey80")
      mtext("Agents", side = 2, line = 1, font = 2, cex = 1)
      mtext(bquote("c = (" * .(paste(cw, collapse = ", ")) * ")  with  " * eta * " = (" * .(paste(eta, collapse = ", ")) * ")"),
            side = 1, line = 1, font = 2, cex = 1)}
    else {
      mosaicplot(results, col = colors, las = 1, cex.axis = 0.9, ylab = "",main = "",border = "grey80")
    }
  }

  # Se devuelve un data.frame con los resultados
  return(results)
}
