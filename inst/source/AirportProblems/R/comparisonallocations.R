#' Graphical evaluation of the contribution vectors
#'
#' \code{comparisonallocations} generates a graphical representation in which, for each agent or group of clones,
#' both the maximum cost they can bear and their corresponding marginal contribution are displayed.
#'
#' @param c A numeric cost vector.
#' @param contributions A list containing the different cost allocation vectors to be compared.
#' It is required that the sum of the coordinates of each vector equals the total cost to be allocated.
#' @param col A character string reflecting the color of the NS constraint for each agent. By default, the color \code{"dodgerblue"} is used.
#' @param colors A vector that indicates the colors used to represent each contribution vector. By default, a color palette of different shades is used.
#' @param agents_names A vector defining the name assigned to each agent. By default, the names follow a sequence of natural numbers, starting from 1.
#' @param labels A logical value indicating whether the labels and the title of the plot should be displayed. By default, \code{labels = TRUE}.
#' @param legend A vector or list where each of its elements represents a different contribution vector.
#' By default, the coordinates of each contribution vector are displayed with two decimal places.
#' @param tol Tolerance level for evaluating compliance with the NS constraint.
#'
#' @return A vertical line plot in which each line represents the maximum amount an agent can pay without violating the NS constraint,
#' while the points along the lines indicate the contributions made by the agent.
#'
#' @details
#' For each \eqn{c\in C^N} let \eqn{H(c)=\{x\in\mathbb{R}:x(N)=c_n\}} be the hyperplane of \eqn{\mathbb{R}^N}
#' given by all the vectors whose coordinates add up to \eqn{c_n}. A cost allocation for \eqn{c\in C^N} is a vector
#' \eqn{x\in H(c)} such that \eqn{0\leq x\leq c}. The component \eqn{x_i} is the contribution requested from agent \eqn{i}.
#' Let \eqn{X(c)} be the set of cost allocations for \eqn{c\in C^N}.
#'
#' A basic requirement is that at an allocation \eqn{x\in X(c)} on group \eqn{N'\subset N}
#' of agents would subsidize the other agents by contributing more than what the group would have to pay on its own. The no-subsidy constraint
#' for the group \eqn{N'\subset N} is \eqn{x(N')\geq \text{max}\{c_j:j\in N'\}}. The set of cost allocations for \eqn{c\in C^N} that satisfy the no-subsidy
#' constraints, the no-subsidy set for short, is given by:
#' \deqn{
#' NS(c)=\{x\in X(c):x(N')\leq\text{max}\{c_j:j\in N'\}, \text{ for all } N'\subset N\}}
#' \deqn{= \{x\in \mathbb{R}^N:x\geq 0, \ x(N)=c_n, \ x_1+\dots+x_i\leq c_i,\text{ for all }i\in N\backslash \{n\}\}
#' }
#' Thus, the no-subsidy correspondence NS assigns to each \eqn{c\in C^N} the set \eqn{NS(c)}.
#'
#' A rule is a mapping \eqn{\mathcal{R}:C^N\rightarrow \mathbb{R}^N} which associates with each problem \eqn{c\in C^N} a contribution vector
#' \eqn{\mathcal{R}(c)\in X(c)}. In other words, a rule is a mechanism that, for each airport problem, selects an allocation vector belonging to its no-subsidy set.
#'
#' @references
#' Bernárdez Ferradás, A., Mirás Calvo, M. Á., Quinteiro Sandomingo, C., and Sánchez-Rodríguez, E. (2025). Airport problems with cloned agents. [Preprint manuscript].
#'
#' Thomson, W. (2024). Cost allocation and airport problems.
#' \emph{Mathematical Social Sciences}, 31(C), 17–31.
#'
#' @examples
#' # CEB rule vs weighted CEB rule vs clones CEB rule
#' c <- c(1, 3, 7, 10) # Cost vector
#' w <- c(1, 4, 8, 2) # Weight vector
#' eta <- w # Size of each groups of clones
#' CEB <- basicrule(c, "CEB")
#' wCEB <- weightedrule(c, w, "CEB")
#' clCEB <- clonesrule(c, w, "CEB")
#' comparisonallocations(c, list(CEB, wCEB, clCEB))
#'
#' # SEC rule vs CEC rule vs SM rule
#' c <- c(5, 10, 20) # Cost vector
#' comparisonallocations(c, list(SECrule(c), CECrule(c), SMrule(c)),
#' col = "green", agents_names = c("Alex", "Estela", "Carmen"), labels = FALSE,
#' legend = list("SEC", "CEC", "SM"))
#'
#' @seealso
#' \code{\link{NScheck}}, \code{\link{basicrule}}, \code{\link{plotallocations}}
#'
#' @importFrom graphics axis grid
#'
#' @export
comparisonallocations <- function(c, contributions, col = NULL, colors = NULL, agents_names = NULL, labels = TRUE, legend = NULL, tol = 1e-6) {
  # c: vector de costes de los agentes
  # contributions: lista que contiene todos los vectores de asignaciones a comparar
  # col: color definido por defecto para la restricción NS. El usuario puede cambiarlo
  # colors: paleta de colores predefinida por defecto. El usuario puede cambiarla
  # agents_names: Por defecto, NULL (agentes numerados en orden crecientes desde el 1)
  # labels: Por defecto, TRUE
  # legend: Definida por defecto, pero el usuario la puede modificar o incluso eliminar
  # tol: nivel de tolerancia prefijado

  # Verificación de que 'c' es un vector numérico
  if (!is.numeric(c)){
    stop("'c' must be a numeric vector")
  }

  # Verificación de que todos los costes sean no negativos (c >= 0)
  if (any(c < 0)) {
    stop("'c' must have nonnegative coordinates")
  }

  ## Requerimiento: Al menos uno de los costes ha de ser positivo
  if (all(c == 0)) {
    stop("'c' must have at least one positive coordinate")
  }

  # Si 'contributions' es un único vector numérico se transforma en una lista
  if (is.numeric(contributions) && is.vector(contributions)) {
    contributions <- list(contributions)
  }

  # Verificación de que 'contributions' sea una lista
  if (!is.list(contributions)) {
    stop("'contributions' must be a list of vectors")
  }

  # Verificación de que cada elemento en 'contributions' sea un vector numérico
  if (any(sapply(contributions, function(x) !is.numeric(x)))) {
    stop("Each element in 'contributions' must be a numeric vector")
  }

  # Verificación de que cada vector de contribuciones tiene la misma longitud que 'c'
  if (any(sapply(contributions, length) != length(c))) {
    stop("Each vector in 'contributions' must have the same length as 'c'")
  }

  # Lista para almacenar los índices de los elementos de 'contributions' que no cumplen con NS
  failed_indices <- c()

  # Comprobación de si los elementos de 'contributions' verifican la restricción NS
  for (j in seq_along(contributions)) {
    tryCatch(
      {
        valid <- NS(contributions[[j]], c, tol = tol)
        if (!valid) {
          failed_indices <- c(failed_indices, j)
        }
      },
      error = function(e) {
        # Incluir el nombre completo de la función en el mensaje de error sin encabezado adicional
        stop(sprintf("Error en comparisonrules(%s, %s): %s", toString(c), toString(contributions), e$message), call. = FALSE)
      }
    )
  }

  # Si hay contribuciones que fallaron, emitir un warning
  if (length(failed_indices) > 0) {
    warning(sprintf("The elements %s, do not satisfy the NS constraint", paste(failed_indices, collapse = ", ")))
  }

  # Si todas las contribuciones superan NS, se continúa con la creación del gráfico
  if (is.null(col)) {
    col <- "dodgerblue"
  }

  if (!is.character(col) || length(col) != 1) {
    stop("'col' must be a single value")
  }

  invalid_color <- c()
  is_valid_color_ <- tryCatch({ col2rgb(col); TRUE }, error = function(e) { FALSE })
  if (!is_valid_color_) {
    invalid_color <- c(invalid_color, col)
  }
  if (length(invalid_color) > 0) {
    stop(paste("The color '", col, "' is not valid", sep = ""))
  }

  col_ <- adjustcolor(col, red.f = 0.8, green.f = 0.8, blue.f = 0.8)

  if (is.null(colors)) {
    colors <- c("darkorchid", "goldenrod", "indianred", "deeppink", "orangered","aquamarine4",
                "seagreen1", "chocolate", "bisque3", "brown", "magenta", "coral", "darkolivegreen","red",
                "darkslategray", "firebrick", "gold", "green", "hotpink", "khaki3", "maroon", "olivedrab",
                "orange", "orchid", "purple", "salmon", "violetred", "tan3")
  } else {
    # La lista "contributions" y el vector "colors" han de tener la misma longitud
    if (length(contributions) != length(colors)) {
      stop("'contributions' and 'colors' must be the same length")
    }
    # Se comprueban que los elementos del vector 'colors' son válidos
    invalid_colors <- c()
    for (i in 1:length(colors)) {
      is_valid_color <- tryCatch({col2rgb(colors[i]); TRUE}, error = function(e) {FALSE})
      if (!is_valid_color) {
        invalid_colors <- c(invalid_colors, colors[i])
      }
    }
    if (length(invalid_colors) > 0) {
      stop(paste("The following colors are not valid: ", paste(invalid_colors, collapse = ", "), sep = ""))
    }
  }

  # Se comprueba que 'labels' es un argumento de tipo lógico
  if (!is.logical(labels) || length(labels) != 1) {
    stop("'labels' must be a single logical value (TRUE or FALSE)")
  }

  # Se revisa si 'tol' es un argumento de tipo numérico
  if (!is.numeric(tol) || length(tol) != 1) {
    stop("'tol' must be a single numeric value")
  }

  # Se crea un gráfico vacío, ampliando los márgenes en el eje X
  if (labels == TRUE) {
    plot(NULL, type = "n", xlab = "Agents", ylab = "Contributions",
         main = paste("Comparison rules for c = (", paste(c, collapse = ", "), ")", sep = ""),
         xlim = c(0.5, length(c) + 0.5), ylim = c(0, max(c, unlist(contributions))),
         xaxt = "n", yaxt = "n")
  } else {
    plot(NULL, type = "n", xlab = "", ylab = "", main = "",
         xlim = c(0.5, length(c) + 0.5), ylim = c(0, max(c, unlist(contributions))),
         xaxt = "n", yaxt = "n")
  }

  # Se agregan los valores del eje X con el nuevo margen
  if (is.null(agents_names)) {
    axis(1, at = 1:length(c), labels = 1:length(c))
  } else {
    if (length(agents_names) != length(c)) {
      stop("'agent_names' and 'c' must have the same length.")
    }
    axis(1, at = 1:length(c), labels = agents_names)
  }

  # Se añade una cuadrícula estándar para facilitar la visualización de la gráfica
  grid(nx = NULL, ny = NULL, col = "lightskyblue1", lty = "dotted")

  # Se representa el vector de costes
  for (i in seq_along(c)) {
    lines(c(i, i), c(0, c[i]), col = col, lty = 2, lwd = 2)
    points(i, c[i], pch = 19, col = col_, cex = 1.5)
  }

  # Se añaden los puntos de cada vector de contribuciones
  for (i in seq_along(c)) {
    contrib_values <- sapply(contributions, function(x) x[i])
    sorted_values <- sort(unique(contrib_values))

    # Agrupar valores similares dentro de la tolerancia
    unique_values <- sorted_values[c(TRUE, diff(sorted_values) > tol)]

    for (j in seq_along(unique_values)) {
      close_values <- which(abs(contrib_values - unique_values[j]) <= tol)
      n_duplicates <- length(close_values)

      shifts <- if (n_duplicates > 1) {
        seq(-0.03 * n_duplicates, 0.03 * n_duplicates, length.out = n_duplicates)
      } else {
        rep(0, n_duplicates)
      }

      for (k in seq_along(close_values)) {
        idx <- close_values[k]
        ns_valid <- NS(contributions[[idx]], c, tol = tol)  # Evaluar NS para este vector

        pch_value <- if (ns_valid) 20 else 18  # Si NS es TRUE, usar pch=20; si es FALSE, usar pch=8

        points(i + shifts[k], contributions[[idx]][i], pch = pch_value, col = colors[idx], cex = 1.6)
      }
    }
  }

  # Se agregan los valores del eje X
  if (is.null(agents_names)) {
    axis(1, at = 1:length(c), labels = 1:length(c))
  } else {
    if (length(agents_names) != length(c)) {
      stop("'agents_names' and 'c' must have the same length.")
    }
    axis(1, at = 1:length(c), labels = agents_names)
  }

  # Se agregan los valores del eje Y
  yticks <- pretty(c(0, max(c, unlist(contributions))))
  axis(2, at = yticks, labels = yticks, las = 2)

  # Verificación y configuración de la leyenda
  if (is.null(legend)) {
    # Determinar qué contribuciones cumplen NS y cuáles no
    ns_flags <- sapply(contributions, function(x) NS(x, c, tol = tol))

    # Asignar los símbolos de la leyenda según si cumplen NS o no
    pch_values <- ifelse(ns_flags, 20, 18)

    # Crear etiquetas con los vectores de contribuciones redondeados
    legend_labels <- lapply(contributions, function(x) paste0("(", paste(round(x, 2), collapse = ", "), ")"))

    # Dibujar la leyenda con los distintos símbolos
    legend("topleft", legend = legend_labels, col = colors, pch = pch_values, text.col = colors, bty = "n")
  } else if (length(legend) == 1 && legend == "") {
    # No dibujar la leyenda si se pasa una cadena vacía
  } else {
    # Usar una leyenda personalizada con los símbolos adecuados
    ns_flags <- sapply(contributions, function(x) NS(x, c, tol = tol))
    pch_values <- ifelse(ns_flags, 20, 18)

    legend("topleft", legend = legend, col = colors, pch = pch_values, text.col = colors, bty = "n")
  }
}
