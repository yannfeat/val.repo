#' Graphical representation of the contribution vectors within the NS set
#'
#' \code{plotallocations} generates a graphical representation of the contribution vectors inside the NS set
#' in 1D, 2D, and 3D (available only when there are 2, 3, or 4 agents).
#'
#' @param c A numeric cost vector.
#' @param contributions A list containing different cost allocation vectors.
#' @param dimension A character string that specifies the dimension of the graphic. Possible values are \code{"1D"}, \code{"2D"}, and \code{"3D"}.
#' By default, the dimension is chosen based on the number of agents: \code{"1D"} for 2 agents, \code{"2D"} for 3 agents, and \code{"3D"} for 4 agents.
#' @param representation A character string indicating which NS set and allocations are displayed. Possible values are \code{"real"}, \code{"projection"}, and \code{"both"}. By default, \code{representation = "projection"}.
#' @param col A character string reflecting the color tone of the NS set. By default, the color tone \code{"dodgerblue"} is used.
#' @param colors A vector that indicates the colors used to represent each contribution vector. By default, a color palette of different shades is used.
#' @param agents_names A vector defining the name assigned to each agent. By default, the names follow a sequence of natural numbers, starting from 1.
#' @param labels A logical value indicating whether the coordinates of the points and the plot title should be displayed. By default, \code{labels = TRUE}.
#' @param contributions_names A vector defining the name assigned to each cost allocation vector. By default, and whenever \code{labels = TRUE}, the Cartesian coordinates of each point are displayed.
#' @param tol Tolerance level for evaluating compliance with the NS constraint.
#'
#' @return Only if the number of agents is 2, 3, or 4 will a plot be generated displaying the NS set and all cost allocation vectors.
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
#' González-Díaz, J., Mirás Calvo, M. Á., Quinteiro Sandomingo, C., and Sánchez-Rodríguez, E. (2016). Airport games: the core and its center. \emph{Mathematical Social Sciences}, 82, 105–115.
#'
#' Mirás Calvo, M. Á., Quinteiro Sandomingo, C., and Sánchez-Rodríguez, E. (2020). The boundary of the core of a balanced game: faces games.
#' \emph{International Journal of Game Theory}, 49(2), 579-599.
#'
#' @examples
#' # Projected SEC rule, CEC rule and SM rule for 3 agents
#' c <- c(5, 10, 20) # Cost vector
#' plotallocations(c, list(SECrule(c), CECrule(c), SMrule(c)), "2D",
#' "projection", contributions_names = c("SEC", "CEC", "SM"))
#'
#' # Real an projected SM rule and PRIOR rule for 3 agentes
#' c <- c(1, 2, 3) # Cost vector
#' SM <- SMrule(c)
#' PRIOR <- PRIORrule(c, order = c(2, 3, 1))
#' plotallocations(c, list(SM, PRIOR), "3D", "both")
#'
#' # Projected CEB rule and weighted CEB rule for 4 agents
#' c <- c(3, 3, 3, 10) # Cost vector
#' w <- c(1, 4, 8, 2) # Weight vector
#' CEB <- basicrule(c, "CEB")
#' wCEB <- weightedrule(c, w, "CEB")
#' plotallocations(c, list(CEB, wCEB), contributions_names = c("CEB", "wCEB"))
#'
#' @seealso
#' \code{\link{NSset}}, \code{\link{NScheck}}, \code{\link{comparisonallocations}}
#'
#' @import plotly
#' @export
plotallocations <- function(c, contributions, dimension = NULL, representation = "projection", col = NULL, colors = NULL,
                            agents_names = NULL, labels = TRUE, contributions_names = NULL, tol = 1e-6) {
  # c: vector de costes de los agentes
  # contributions: lista que contiene todos los vectores de asignaciones a comparar
  # dimension: Dimensión bajo la que se construye el gráfico ("1D", "2D" o "3D")
  # representation: Figura que se representa ("real", "projection" or "both")
  # col: color definido por defecto (col1, col2 y col3 se definen en base a col). El usuario puede cambiarlo
  # colors: paleta de colores predefinida por defecto. El usuario puede cambiarla
  # agents_names: Por defecto, NULL (agentes numerados en orden crecientes desde el 1)
  # labels: Por defecto, TRUE (se muestran etiquetas de los vértices y el título)
  # contributions_names: Por defecto, NULL (coordenadas de los puntos)
  # tol: nivel de tolerancia prefijado

  # Verificación de que 'c' es un vector numérico
  if (!is.numeric(c)){
    stop("'c' must be a numeric vector")
  }

  ## Requerimiento: Todos los costes del vector 'c' han de ser no negativos
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
        stop(sprintf("Error en plotallocations(%s, %s): %s", toString(c), toString(contributions), e$message), call. = FALSE)
      }
    )
  }

  # Si hay contribuciones que fallaron, emitir un warning
  if (length(failed_indices) > 0) {
    stop(sprintf("The elements %s, do not satisfy the NS constraint", paste(failed_indices, collapse = ", ")))
  }

  original.c <- c # Vector de costes original
  original.order <- order(c) # Orden original de los costes
  contributions <- lapply(contributions, function (x) x[order(c)]) # Ordenación de las contribuciones en base al orden creciente de c
  c <- sort(c) # Ordenación de los agentes en orden creciente
  n <- length(c) # Número de agentes

  # Dimension=NULL y n=2:
  if (is.null(dimension) && n == 2){
    dimension <- "1D"
  }

  # Dimension=NULL y n=3:
  if (is.null(dimension) && n == 3){
    dimension <- "2D"
  }

  # Dimension=NULL y n=3:
  if (is.null(dimension) && n == 4){
    dimension <- "3D"
  }

  # Verificación de que 'dimension' es válido
  if (!dimension %in% c("1D", "2D", "3D")) {
    stop("'dimension' must be '1D', '2D' or '3D'.")
  }

  # Verificación de que 'representation' es válido
  if (!representation %in% c("real", "projection", "both")) {
    stop("'representation' must be 'real', 'projection' or 'both'.")
  }

  # Chequeo del color a utilizar
  if (is.null(col)) {
    col <- "dodgerblue"
  } else {
    is_valid_color <- tryCatch({col2rgb(col); TRUE}, error = function(e) {FALSE})
    if (!is_valid_color) {
      stop(paste("'", col, "' is not a valid color", sep = ""))
    }
  }

  if (is.null(colors)) {
    colors <- c("darkorchid", "goldenrod", "indianred", "deeppink", "orangered","aquamarine4",
                "seagreen1", "chocolate", "bisque3", "brown", "magenta", "coral", "darkolivegreen","red",
                "darkslategray", "firebrick", "gold", "green", "hotpink", "khaki3", "maroon", "olivedrab",
                "orange", "orchid", "purple", "salmon", "violetred", "tan3")
  } else {
    # La lista "contributions" y el vector "col2" han de tener la misma longitud
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

  shadow_colors <- adjustcolor(colors, red.f = 0.7, green.f = 0.7, blue.f = 0.7)

  # Se comprueba que 'labels' es un argumento de tipo lógico
  if (!is.logical(labels) || length(labels) != 1) {
    stop("'labels' must be a single logical value (TRUE or FALSE)")
  }

  # Se revisa si 'tol' es un argumento de tipo numérico
  if (!is.numeric(tol) || length(tol) != 1) {
    stop("'tol' must be a single numeric value")
  }

  # Caso n=1
  if (n == 1) {
    return(paste('The allocations vectors are single points'))
  }


  # Caso n>4
  if (n > 4) {
    stop("The allocations vectors cannot be drawn when there are more than 4 agents")
  }

  # Sucesión de Warnings
  if ((n == 2 && dimension == "1D" && representation == "both") ||
      (n == 2 && dimension == "2D" && representation == "projection") ||
      (n == 2 && dimension == "3D") ||
      (n == 3 && dimension == "1D") ||
      (n == 3 && dimension == "2D" && representation == "both") ||
      (n == 3 && dimension == "3D" && representation == "projection") ||
      (n == 4 && dimension == "1D") ||
      (n == 4 && dimension == "2D") ||
      (n == 4 && dimension == "3D" && representation == "both"))
  {
    stop("The allocations vectors cannot be drawn for this case")
  }


  col1 = adjustcolor(col, offset = c(-0.1, -0.1, -0.1, 0))
  col2 = adjustcolor(col, offset = c(-0.25, -0.25, -0.25, 0))
  col3 = adjustcolor(col, red.f = 0.7, green.f = 0.7, blue.f = 0.7)

  # Reoordenación de 'agents_names' en base al nuevo orden de los costes
  agents <- agents_names[original.order]

  # Caso n=2
  if (n == 2) {

    # Proyectado y 1D
    if (dimension == "1D" && representation == "projection") {

      # El primer coste es 0
      if (any(c == 0)) {
        # Se dibuja el gráfico con los límites ajustados
        x1 <- seq(0, 0.5, length.out = 50)
        x2 <- rep(median(x1), length(x1))
        if (labels == TRUE) {
          plot(x1, x2, type = "l", col = adjustcolor("black", alpha.f = 0.4), xlab = "", ylab = "", axes = FALSE,
               main = paste("Allocations for c = (", paste(c, collapse = ", "), ")", sep = ""),
               lwd = 1.6, lty = "longdash", xlim = range(x1), ylim = range(x2))}
        else {
          plot(x1, x2, type = "l", col = adjustcolor("black", alpha.f = 0.4), xlab = "", ylab = "", axes = FALSE,
               lwd = 1.6, lty = "longdash", xlim = range(x1), ylim = range(x2))}

        # Punto que delimita el NS check
        points(x1[1], median(x1), col = adjustcolor(col1, alpha.f = 0.6), pch = 20)
        for (j in 1:length(contributions)) {
          points(contributions[[j]][1], median(x1), col = colors[j %% length(colors)], pch = 19, cex = 1.2)
          if (labels == TRUE) {
            label_text <- if (!is.null(contributions_names)) {
              contributions_names[j]
            } else {
              contributions[[j]][1]
            }
            text(contributions[[j]][1], median(x1), labels = label_text, pos = 3,
                 col = colors[j %% length(colors)], font = 2, cex = 1.1)
          }
        }

        # Coordenadas específicas de los ejes
        if (labels == TRUE) {
          text(x1[1], x2, labels = 0, pos = 1, col = adjustcolor(col2, alpha.f = 0.6), cex = 0.9)}
        if (is.null(agents_names)) {
          text(0.98*x1[50], x2, labels = expression(x[1]), pos = 1, col = adjustcolor("black", alpha.f = 0.4))}
        else {
          text(0.95*x1[50], x2, labels = agents[1], pos = 1, col = adjustcolor("black", alpha.f = 0.4))
        }
      }

      # El primer coste es positivo

      else {
        # Se dibuja el gráfico con los límites ajustados
        x1 <- seq(0, c[1], length.out = 50)
        x2 <- rep(median(x1), length(x1))
        if (labels == TRUE) {
          plot(x1, x2, type = "l", col = adjustcolor("black", alpha.f = 0.4), xlab = "", ylab = "", axes = FALSE,
               main = paste("Allocations for c = (", paste(c, collapse = ", "), ")", sep = ""),
               lwd = 1.6, lty = "longdash", xlim = range(x1), ylim = range(x2))}
        else {
          plot(x1, x2, type = "l", col = adjustcolor("black", alpha.f = 0.4), xlab = "", ylab = "", axes = FALSE,
               lwd = 1.6, lty = "longdash", xlim = range(x1), ylim = range(x2))}

        # Líneas que lo conforman
        lines(x1, x2, col = adjustcolor(col, alpha.f = 0.6), lwd = 3)

        # Puntos que delimita el NS check
        points(x1[1], median(x1), col = adjustcolor(col1, alpha.f = 0.6), pch = 20)
        points(x1[50], median(x1), col = adjustcolor(col1, alpha.f = 0.6), pch = 20)
        for (j in 1:length(contributions)) {
          points(contributions[[j]][1], median(x1), col = colors[j %% length(colors)], pch = 19, cex = 1.2)
          if (labels == TRUE) {
            label_text <- if (!is.null(contributions_names)) {
              contributions_names[j]
            } else {
              contributions[[j]][1]
            }
            text(contributions[[j]][1], median(x1), labels = label_text, pos = 3,
                 col = colors[j %% length(colors)], font = 2, cex = 1.1)
          }
        }

        # Coordenadas específicas de los ejes
        if (labels == TRUE) {
          text(x1[1], x2, labels = 0, pos = 1, col = adjustcolor(col2, alpha.f = 0.6), cex = 0.9)
          text(x1[50], x2, labels = c[1], pos = 1, col = adjustcolor(col2, alpha.f = 0.6), cex = 0.9)}
        if (is.null(agents_names)){
          text(x1[50]/2, x2, labels = expression(x[1]), pos = 1, col = adjustcolor("black", alpha.f = 0.4))}
        else {
          text(x1[50]/2, x2, labels = agents[1], pos = 1, col = adjustcolor("black", alpha.f = 0.4))
        }
      }
    }

    # Real y 1D
    else if (dimension == "1D" && representation == "real") {

      # El primer coste es 0 y el otro positivo
      if (any(c == 0)) {
        # Se dibuja el gráfico con los límites ajustados
        if (labels == TRUE) {
          plot(c[1], c[2], type="n", xlim = c(0, 0.65 * c[2]), ylim = c(0, 1.2 * c[2]),
               xlab = "", ylab = "",
               main = paste("Real NS set for c = (", paste(c, collapse = ", "), ")", sep = ""),
               axes = FALSE, frame.plot = FALSE)}
        else {
          plot(c[1], c[2], type="n", xlim = c(0, 0.65 * c[2]), ylim = c(0, 1.2 * c[2]),
               xlab = "", ylab = "", axes = FALSE, frame.plot = FALSE)
        }

        # Punto que delimita el NS check
        points(0.05 * c[2], 1.05 * c[2], col = col1, pch = 20, cex = 1.3)
        for (j in 1:length(contributions)) {
          points(0.05 * contributions[[j]][2], 1.05 * contributions[[j]][2], col = colors[j %% length(colors)], pch = 19, cex = 1.2)
          if (labels == TRUE) {
            label_text <- if (!is.null(contributions_names)) {
              contributions_names[j]
            } else {
              paste0("(", contributions[[j]][1], ", ", contributions[[j]][2], ")")
            }
            text(0.05 * contributions[[j]][2], 1.05 * contributions[[j]][2],
                 labels = label_text, pos = 3,
                 col = colors[j %% length(colors)], font = 2, cex = 1.1)
          }
        }

        # Coordenadas específicas de los ejes
        if (labels == TRUE) {
          text(0.05 * c[2], 1.05 * c[2], labels = paste("(0, ", c[2], ")", sep = ""), pos = 1, col = adjustcolor(col2, alpha.f = 0.6), cex = 0.9)}
      }

      # Ambos costes son iguales
      else if (c[1] == c[2]) {
        # Se dibuja el gráfico con los límites ajustados
        if (labels == TRUE) {
          plot(c[1], c[2], type="n", xlim = c(-0.2 * c[1], 1.2 * c[1]), ylim = c(0, 1.2 * c[2]),
               xlab = "", ylab = "",
               main = paste("Real NS set for c = (", paste(c, collapse = ", "), ")", sep = ""),
               axes = FALSE, frame.plot = FALSE)}
        else {
          plot(c[1], c[2], type="n", xlim = c(-0.2 * c[1], 1.2 * c[1]), ylim = c(0, 1.2 * c[2]),
               xlab = "", ylab = "", axes = FALSE, frame.plot = FALSE)
        }

        # Líneas que lo conforman
        lines(c(0, c[1]), c(c[2], c[2] - c[1]), col = adjustcolor(col1, alpha.f = 0.6), lwd = 3)

        # Puntos que delimitan el NS check
        points(0, c[2], col = adjustcolor(col1, alpha.f = 0.6), pch = 20)
        points(c[1], c[2] - c[1], col = adjustcolor(col1, alpha.f = 0.6), pch = 20)
        for (j in 1:length(contributions)) {
          points(contributions[[j]][1], contributions[[j]][2], col = colors[j %% length(colors)], pch = 19, cex = 1.2)
          if (labels == TRUE) {
            label_text <- if (!is.null(contributions_names)) {
              contributions_names[j]
            } else {
              paste0("(", contributions[[j]][1], ", ", contributions[[j]][2], ")")
            }
            text(contributions[[j]][1], contributions[[j]][2],
                 labels = label_text, adj = c(0.3, -1),
                 col = colors[j %% length(colors)], font = 2, cex = 1.1)
          }
        }

        # Coordenadas específicas de los ejes
        if (labels == TRUE) {
          text(c[1], 0, labels = paste("(", c[1], ", 0)", sep = ""), adj = c(1.2, 1), col = adjustcolor(col2, alpha.f = 0.6), cex = 0.9)
          text(0, c[2], labels = paste("(0, ", c[2], ")", sep = ""), adj=c(1, 1.2), col = adjustcolor(col2, alpha.f = 0.6), cex = 0.9)}
      }

      # Los dos costes son positivos y distintos
      else {
        # Se dibuja el gráfico con los límites ajustados
        if (labels == TRUE){
          plot(c[1], c[2], type="n", xlim = c(-0.2 * c[1], 1.2 * c[1]), ylim = c(0, 1.2 * c[2]),
               xlab = "", ylab = "",
               main = paste("Real NS set for c = (", paste(c, collapse = ", "), ")", sep = ""),
               axes = FALSE, frame.plot = FALSE)}
        else {
          plot(c[1], c[2], type="n", xlim = c(-0.2 * c[1], 1.2 * c[1]), ylim = c(0, 1.2 * c[2]),
               xlab = "", ylab = "", axes = FALSE, frame.plot = FALSE)
        }

        # Líneas que lo conforman
        lines(c(0, c[1]), c(c[2], c[2] - c[1]), col = adjustcolor(col, alpha.f = 0.6), lwd = 3)

        # Puntos que delimitan el NS check
        points(0, c[2], col = adjustcolor(col1, alpha.f = 0.6), pch = 20)
        points(c[1], c[2] - c[1], col = adjustcolor(col1, alpha.f = 0.6), pch = 20)
        for (j in 1:length(contributions)) {
          points(contributions[[j]][1], contributions[[j]][2], col = colors[j %% length(colors)], pch = 19, cex = 1.2)
          if (labels == TRUE) {
            label_text <- if (!is.null(contributions_names)) {
              contributions_names[j]
            } else {
              paste0("(", contributions[[j]][1], ", ", contributions[[j]][2], ")")
            }
            text(contributions[[j]][1], contributions[[j]][2],
                 labels = label_text, adj = c(0.3, -1),
                 col = colors[j %% length(colors)], font = 2, cex = 1.1)
          }
        }

        # Coordenadas específicas de los ejes
        if (labels == TRUE) {
          text(c[1], c[2] - c[1], labels = paste("(", c[1], ", ", c[2] - c[1], ")", sep = ""), adj = c(1.2, 1), col = adjustcolor(col2, alpha.f = 0.6), cex = 0.9)
          text(0, c[2], labels = paste("(0, ", c[2], ")", sep = ""), adj=c(1, 1.2), col = adjustcolor(col2, alpha.f = 0.6), cex = 0.9)}
      }
    }

    # Real y 2D
    else if (dimension == "2D" && representation == "real") {

      # El primer coste es 0 y el otro positivo
      if (any(c == 0)) {
        # Se dibuja el gráfico con los límites ajustados
        if (labels == TRUE) {
          plot(c[1], c[2], type="n", xlim = c(-0.2 * c[2], 0.65 * c[2]), ylim = c(0, 1.2 * c[2]),
               xlab = "", ylab = "",
               main = paste("Real NS set for c = (", paste(c, collapse = ", "), ")", sep = ""),
               axes = FALSE, frame.plot = FALSE)}
        else {
          plot(c[1], c[2], type="n", xlim = c(-0.2 * c[2], 0.65 * c[2]), ylim = c(0, 1.2 * c[2]),
               xlab = "", ylab = "", axes = FALSE, frame.plot = FALSE)
        }

        # Líneas que lo conforman
        lines(c(0, 0.7 * c[2]), c(0, 0), lty = "dashed", lwd = 1.6, col = adjustcolor("black", alpha.f = 0.4))
        lines(c(0, 0), c(0, 1.2 * c[2]), lty = "longdash", lwd = 1.6, col = adjustcolor("black", alpha.f = 0.4))
        lines(c(0, 0), c(0, c[2]), lwd = 1.6, col = adjustcolor("black", alpha.f = 0.4))

        # Punto que delimita el NS check
        points(0, c[2], col = adjustcolor(col1, alpha.f = 0.6), pch = 20)
        for (j in 1:length(contributions)) {
          points(contributions[[j]][1], contributions[[j]][2], col = colors[j %% length(colors)], pch = 19, cex = 1.2)
          if (labels == TRUE) {
            label_text <- if (!is.null(contributions_names)) {
              contributions_names[j]
            } else {
              paste0("(", contributions[[j]][1], ", ", contributions[[j]][2], ")")
            }
            text(contributions[[j]][1], contributions[[j]][2],
                 labels = label_text, pos = 4,
                 col = colors[j %% length(colors)], font = 2, cex = 1.1)
          }
        }

        # Coordenadas específicas de los ejes
        if (labels == TRUE) {
          text(0, 0, labels = expression("(0, 0)"), adj = c(1.1, 1.1), col = adjustcolor("black", alpha.f = 0.6), cex = 0.9)
          text(0, c[2], labels = paste("(0, ", c[2], ")", sep = ""), adj = c(1.1, 1), col = adjustcolor(col2, alpha.f = 0.6), cex = 0.9)}
        if (is.null(agents_names)) {
          text(0.65*c[2], 0.01*c[2], labels = expression(x[1]), pos = 1, col = adjustcolor("black", alpha.f = 0.4))
          text(0, 1.18*c[2], labels = expression(x[2]), pos = 2, col = adjustcolor("black", alpha.f = 0.4))}
        else {
          text(0.65*c[2], 0.01*c[2], labels = agents[1], pos = 1, col = adjustcolor("black", alpha.f = 0.4))
          text(0, 1.18*c[2], labels = agents[2], pos = 2, col = adjustcolor("black", alpha.f = 0.4))
        }
      }

      # Ambos costes son iguales
      else if (c[1] == c[2]) {
        # Se dibuja el gráfico con los límites ajustados
        if (labels == TRUE) {
          plot(c[1], c[2], type="n", xlim = c(-0.2 * c[1], 1.2 * c[1]), ylim = c(0, 1.2 * c[2]),
               xlab = "", ylab = "",
               main = paste("Real NS set for c = (", paste(c, collapse = ", "), ")", sep = ""),
               axes = FALSE, frame.plot = FALSE)}
        else {
          plot(c[1], c[2], type="n", xlim = c(-0.2 * c[1], 1.2 * c[1]), ylim = c(0, 1.2 * c[2]),
               xlab = "", ylab = "", axes = FALSE, frame.plot = FALSE)
        }

        # Líneas que lo conforman
        lines(c(0, 1.2 * c[1]), c(0, 0), lty = "longdash", lwd = 1.6, col = adjustcolor("black", alpha.f = 0.4))
        lines(c(0, 0), c(0, 1.2 * c[2]), lty = "longdash", lwd = 1.6, col = adjustcolor("black", alpha.f = 0.4))
        lines(c(0, c[1]), c(0, 0), lwd = 1.6, col = adjustcolor("black", alpha.f = 0.4))
        lines(c(0, 0), c(0, c[2]), lwd = 1.6, col = adjustcolor("black", alpha.f = 0.4))
        lines(c(c[1], c[1]), c(c[2] - c[1], 0), lwd = 1.6, col = adjustcolor("black", alpha.f = 0.4))
        lines(c(0, c[1]), c(c[2], c[2] - c[1]), col = adjustcolor(col, alpha.f = 0.6), lwd = 3)

        # Puntos que delimitan el NS check
        points(0, c[2], col = adjustcolor(col1, alpha.f = 0.6), pch = 20)
        points(c[1], c[2] - c[1], col = adjustcolor(col1, alpha.f = 0.6), pch = 20)
        for (j in 1:length(contributions)) {
          points(contributions[[j]][1], contributions[[j]][2], col = colors[j %% length(colors)], pch = 19, cex = 1.2)
          if (labels == TRUE) {
            label_text <- if (!is.null(contributions_names)) {
              contributions_names[j]
            } else {
              paste0("(", contributions[[j]][1], ", ", contributions[[j]][2], ")")
            }
            text(contributions[[j]][1], contributions[[j]][2],
                 labels = label_text, adj = c(-0.2, -0.3),
                 col = colors[j %% length(colors)], font = 2, cex = 1.1)
          }
        }

        # Coordenadas específicas de los ejes
        if (labels == TRUE) {
          text(0, 0, labels = expression("(0, 0)"), adj = c(1.1, 1.1), col = adjustcolor("black", alpha.f = 0.6), cex = 0.9)
          text(0, c[2], labels = paste("(0, ", c[2], ")", sep = ""), adj = c(1.1, 1), col = adjustcolor(col2, alpha.f = 0.6), cex = 0.9)
          text(c[1], 0, labels = paste("(", c[1], ", 0)", sep = ""), adj = c(0.7, 1.2), col = adjustcolor(col2, alpha.f = 0.6), cex = 0.9)
        }
        if (is.null(agents_names)) {
          text(1.18*c[1], 0.01 * c[2], labels = expression(x[1]), pos = 1, col = adjustcolor("black", alpha.f = 0.4))
          text(0, 1.18*c[2], labels = expression(x[2]), pos = 2, col = adjustcolor("black", alpha.f = 0.4))}
        else {
          text(1.18*c[1], 0.01 * c[2], labels = agents[1], pos = 1, col = adjustcolor("black", alpha.f = 0.4))
          text(0, 1.18*c[2], labels = agents[2], pos = 2, col = adjustcolor("black", alpha.f = 0.4))
        }
      }
      # Los dos costes son positivos y distintos
      else {
        # Se dibuja el gráfico con los límites ajustados
        if (labels == TRUE){
          plot(c[1], c[2], type="n", xlim = c(-0.2 * c[1], 1.2 * c[1]), ylim = c(0, 1.2 * c[2]),
               xlab = "", ylab = "",
               main = paste("Real NS set for c = (", paste(c, collapse = ", "), ")", sep = ""),
               axes = FALSE, frame.plot = FALSE)}
        else {
          plot(c[1], c[2], type="n", xlim = c(-0.2 * c[1], 1.2 * c[1]), ylim = c(0, 1.2 * c[2]),
               xlab = "", ylab = "", axes = FALSE, frame.plot = FALSE)
        }

        # Líneas que lo conforman
        lines(c(0, 1.2 * c[1]), c(0, 0), lty = "longdash", lwd = 1.6, col = adjustcolor("black", alpha.f = 0.4))
        lines(c(0, 0), c(0, 1.2 * c[2]), lty = "longdash", lwd = 1.6, col = adjustcolor("black", alpha.f = 0.4))
        lines(c(0, c[1]), c(0, 0), lwd = 1.6, col = adjustcolor("black", alpha.f = 0.4))
        lines(c(0, 0), c(0, c[2]), lwd = 1.6, col = adjustcolor("black", alpha.f = 0.4))
        if (labels == TRUE) {
          lines(c(c[1], c[1]), c(c[2] - c[1], 0), lwd = 1.6, lty = "dashed", col = adjustcolor("grey15", alpha.f = 0.5))
          lines(c(0, c[1]), c(c[2] - c[1], c[2] - c[1]), lwd = 1.6, lty = "dashed", col = adjustcolor("grey15", alpha.f = 0.5))}
        lines(c(0, c[1]), c(c[2], c[2] - c[1]), col = adjustcolor(col, alpha.f = 0.6), lwd = 3)

        # Puntos que delimitan el NS check
        points(0, c[2], col = adjustcolor(col1, alpha.f = 0.6), pch = 20)
        points(c[1], c[2] - c[1], col = adjustcolor(col1, alpha.f = 0.6), pch = 20)
        for (j in 1:length(contributions)) {
          points(contributions[[j]][1], contributions[[j]][2], col = colors[j %% length(colors)], pch = 19, cex = 1.2)
          if (labels == TRUE) {
            label_text <- if (!is.null(contributions_names)) {
              contributions_names[j]
            } else {
              paste0("(", contributions[[j]][1], ", ", contributions[[j]][2], ")")
            }
            text(contributions[[j]][1], contributions[[j]][2],
                 labels = label_text, adj = c(-0.2, -0.3),
                 col = colors[j %% length(colors)], font = 2, cex = 1.1)
          }
        }

        # Coordenadas específicas de los ejes
        if (labels == TRUE) {
          text(0, 0, labels = expression("(0, 0)"), adj = c(1.1, 1.1), col = adjustcolor("black", alpha.f = 0.6), cex = 0.9)
          text(0, c[2], labels = paste("(0, ", c[2], ")", sep = ""), adj = c(1.1, 1), col = adjustcolor(col2, alpha.f = 0.6), cex = 0.9)
          text(c[1], c[2] - c[1], labels = paste("(", c[1], ", ", c[2] - c[1], ")", sep = ""), adj = c(1.1, 1.5), col = adjustcolor(col2, alpha.f = 0.6), cex = 0.9)
          text(c[1], 0, labels = paste("(", c[1], ", 0)", sep = ""), adj = c(0.7, 1.2), col = adjustcolor("black", alpha.f = 0.6), cex = 0.9)
        }
        if (is.null(agents_names)) {
          text(1.18*c[1], 0.01 * c[2], labels = expression(x[1]), pos = 1, col = adjustcolor("black", alpha.f = 0.4))
          text(0, 1.18*c[2], labels = expression(x[2]), pos = 2, col = adjustcolor("black", alpha.f = 0.4))}
        else {
          text(1.18*c[1], 0.01 * c[2], labels = agents[1], pos = 1, col = adjustcolor("black", alpha.f = 0.4))
          text(0, 1.18*c[2], labels = agents[2], pos = 2, col = adjustcolor("black", alpha.f = 0.4))
        }
      }
    }
    # Ambos y 2D
    else if (dimension == "2D" && representation == "both") {

      # El primer coste es 0 y el otro positivo
      if (any(c == 0)) {
        # Se dibuja el gráfico con los límites ajustados
        if (labels == TRUE) {
          plot(c[1], c[2], type="n", xlim = c(-0.2 * c[2], 0.65 * c[2]), ylim = c(0, 1.2 * c[2]),
               xlab = "", ylab = "",
               main = paste("Real NS set for c = (", paste(c, collapse = ", "), ")", sep = ""),
               axes = FALSE, frame.plot = FALSE)}
        else {
          plot(c[1], c[2], type="n", xlim = c(-0.2 * c[2], 0.65 * c[2]), ylim = c(0, 1.2 * c[2]),
               xlab = "", ylab = "", axes = FALSE, frame.plot = FALSE)
        }

        # Líneas que lo conforman
        lines(c(0, 0.7 * c[2]), c(0, 0), lty = "dashed", lwd = 1.6, col = adjustcolor("black", alpha.f = 0.4))
        lines(c(0, 0), c(0, 1.2 * c[2]), lty = "longdash", lwd = 1.6, col = adjustcolor("black", alpha.f = 0.4))
        lines(c(0, 0), c(0, c[2]), lwd = 1.6, col = adjustcolor("black", alpha.f = 0.4))

        # Punto que delimita el NS check
        points(0, c[2], col = adjustcolor(col1, alpha.f = 0.6), pch = 20)
        points(0, 0, col = adjustcolor(col3, alpha.f = 0.6), pch = 20)
        for (j in 1:length(contributions)) {
          points(contributions[[j]][1], contributions[[j]][2], col = colors[j %% length(colors)], pch = 19, cex = 1.2)
          points(contributions[[j]][1], 0, col = shadow_colors[j %% length(shadow_colors)], pch = 19, cex = 1.2)
          if (labels == TRUE) {
            label_text <- if (!is.null(contributions_names)) {
              contributions_names[j]
            } else {
              paste0("(", contributions[[j]][1], ", ", contributions[[j]][2], ")")
            }
            text(contributions[[j]][1], contributions[[j]][2],
                 labels = label_text, pos = 4,
                 col = colors[j %% length(colors)], font = 2, cex = 1.1)
          }
        }

        # Coordenadas específicas de los ejes
        if (labels == TRUE) {
          text(0, 0, labels = expression("(0, 0)"), adj = c(1.1, 1.1), col = adjustcolor("black", alpha.f = 0.6), cex = 0.9)
          text(0, c[2], labels = paste("(0, ", c[2], ")", sep = ""), adj = c(1.1, 1), col = adjustcolor(col2, alpha.f = 0.6), cex = 0.9)}
        if (is.null(agents_names)) {
          text(0.65*c[2], 0.01*c[2], labels = expression(x[1]), pos = 1, col = adjustcolor("black", alpha.f = 0.4))
          text(0, 1.18*c[2], labels = expression(x[2]), pos = 2, col = adjustcolor("black", alpha.f = 0.4))}
        else {
          text(0.65*c[2], 0.01*c[2], labels = agents[1], pos = 1, col = adjustcolor("black", alpha.f = 0.4))
          text(0, 1.18*c[2], labels = agents[2], pos = 2, col = adjustcolor("black", alpha.f = 0.4))
        }
      }

      # Ambos costes son iguales
      else if (c[1] == c[2]) {
        # Se dibuja el gráfico con los límites ajustados
        if (labels == TRUE) {
          plot(c[1], c[2], type="n", xlim = c(-0.2 * c[1], 1.2 * c[1]), ylim = c(0, 1.2 * c[2]),
               xlab = "", ylab = "",
               main = paste("Real NS set for c = (", paste(c, collapse = ", "), ")", sep = ""),
               axes = FALSE, frame.plot = FALSE)}
        else {
          plot(c[1], c[2], type="n", xlim = c(-0.2 * c[1], 1.2 * c[1]), ylim = c(0, 1.2 * c[2]),
               xlab = "", ylab = "", axes = FALSE, frame.plot = FALSE)
        }

        # Líneas que lo conforman
        lines(c(0, 1.2 * c[1]), c(0, 0), lty = "longdash", lwd = 1.6, col = adjustcolor("black", alpha.f = 0.4))
        lines(c(0, 0), c(0, 1.2 * c[2]), lty = "longdash", lwd = 1.6, col = adjustcolor("black", alpha.f = 0.4))
        lines(c(0, c[1]), c(0, 0), lwd = 1.6, col = adjustcolor("black", alpha.f = 0.4))
        lines(c(0, 0), c(0, c[2]), lwd = 1.6, col = adjustcolor("black", alpha.f = 0.4))
        lines(c(c[1], c[1]), c(c[2] - c[1], 0), lwd = 1.6, col = adjustcolor("black", alpha.f = 0.4))
        lines(c(0, c[1]), c(c[2], c[2] - c[1]), col = adjustcolor(col, alpha.f = 0.6), lwd = 3)

        # Puntos que delimitan el NS check
        points(0, c[2], col = adjustcolor(col1, alpha.f = 0.6), pch = 20)
        points(c[1], c[2] - c[1], col = adjustcolor(col1, alpha.f = 0.6), pch = 20)
        for (j in 1:length(contributions)) {
          points(contributions[[j]][1], contributions[[j]][2], col = colors[j %% length(colors)], pch = 19, cex = 1.2)
          points(contributions[[j]][1], 0, col = shadow_colors[j %% length(shadow_colors)], pch = 19, cex = 1.2)
          if (labels == TRUE) {
            label_text <- if (!is.null(contributions_names)) {
              contributions_names[j]
            } else {
              paste0("(", contributions[[j]][1], ", ", contributions[[j]][2], ")")
            }
            text(contributions[[j]][1], contributions[[j]][2],
                 labels = label_text, adj = c(-0.2, -0.3),
                 col = colors[j %% length(colors)], font = 2, cex = 1.1)
          }
        }

        # Coordenadas específicas de los ejes
        if (labels == TRUE) {
          text(0, 0, labels = expression("(0, 0)"), adj = c(1.1, 1.1), col = adjustcolor("black", alpha.f = 0.6), cex = 0.9)
          text(0, c[2], labels = paste("(0, ", c[2], ")", sep = ""), adj = c(1.1, 1), col = adjustcolor(col2, alpha.f = 0.6), cex = 0.9)
          text(c[1], 0, labels = paste("(", c[1], ", 0)", sep = ""), adj = c(0.7, 1.2), col = adjustcolor(col2, alpha.f = 0.6), cex = 0.9)
        }
        if (is.null(agents_names)) {
          text(1.18*c[1], 0.01 * c[2], labels = expression(x[1]), pos = 1, col = adjustcolor("black", alpha.f = 0.4))
          text(0, 1.18*c[2], labels = expression(x[2]), pos = 2, col = adjustcolor("black", alpha.f = 0.4))}
        else {
          text(1.18*c[1], 0.01 * c[2], labels = agents[1], pos = 1, col = adjustcolor("black", alpha.f = 0.4))
          text(0, 1.18*c[2], labels = agents[2], pos = 2, col = adjustcolor("black", alpha.f = 0.4))
        }
      }
      # Los dos costes son positivos y distintos
      else {
        # Se dibuja el gráfico con los límites ajustados
        if (labels == TRUE){
          plot(c[1], c[2], type="n", xlim = c(-0.2 * c[1], 1.2 * c[1]), ylim = c(0, 1.2 * c[2]),
               xlab = "", ylab = "",
               main = paste("Real NS set for c = (", paste(c, collapse = ", "), ")", sep = ""),
               axes = FALSE, frame.plot = FALSE)}
        else {
          plot(c[1], c[2], type="n", xlim = c(-0.2 * c[1], 1.2 * c[1]), ylim = c(0, 1.2 * c[2]),
               xlab = "", ylab = "", axes = FALSE, frame.plot = FALSE)
        }

        # Líneas que lo conforman
        lines(c(0, 1.2 * c[1]), c(0, 0), lty = "longdash", lwd = 1.6, col = adjustcolor("black", alpha.f = 0.4))
        lines(c(0, 0), c(0, 1.2 * c[2]), lty = "longdash", lwd = 1.6, col = adjustcolor("black", alpha.f = 0.4))
        lines(c(0, c[1]), c(0, 0), lwd = 1.6, col = adjustcolor("black", alpha.f = 0.4))
        lines(c(0, 0), c(0, c[2]), lwd = 1.6, col = adjustcolor("black", alpha.f = 0.4))
        if (labels == TRUE) {
          lines(c(c[1], c[1]), c(c[2] - c[1], 0), lwd = 1.6, lty = "dashed", col = adjustcolor("grey15", alpha.f = 0.5))
          lines(c(0, c[1]), c(c[2] - c[1], c[2] - c[1]), lwd = 1.6, lty = "dashed", col = adjustcolor("grey15", alpha.f = 0.5))}
        lines(c(0, c[1]), c(c[2], c[2] - c[1]), col = adjustcolor(col, alpha.f = 0.6), lwd = 3)

        # Puntos que delimitan el NS check
        points(0, c[2], col = adjustcolor(col1, alpha.f = 0.6), pch = 20)
        points(c[1], c[2] - c[1], col = adjustcolor(col1, alpha.f = 0.6), pch = 20)
        for (j in 1:length(contributions)) {
          points(contributions[[j]][1], contributions[[j]][2], col = colors[j %% length(colors)], pch = 19, cex = 1.2)
          points(contributions[[j]][1], 0, col = shadow_colors[j %% length(shadow_colors)], pch = 19, cex = 1.2)
          if (labels == TRUE) {
            label_text <- if (!is.null(contributions_names)) {
              contributions_names[j]
            } else {
              paste0("(", contributions[[j]][1], ", ", contributions[[j]][2], ")")
            }
            text(contributions[[j]][1], contributions[[j]][2],
                 labels = label_text, adj = c(-0.2, -0.3),
                 col = colors[j %% length(colors)], font = 2, cex = 1.1)
          }
        }

        # Coordenadas específicas de los ejes
        if (labels == TRUE) {
          text(0, 0, labels = expression("(0, 0)"), adj = c(1.1, 1.1), col = adjustcolor("black", alpha.f = 0.6), cex = 0.9)
          text(0, c[2], labels = paste("(0, ", c[2], ")", sep = ""), adj = c(1.1, 1), col = adjustcolor(col2, alpha.f = 0.6), cex = 0.9)
          text(c[1], c[2] - c[1], labels = paste("(", c[1], ", ", c[2] - c[1], ")", sep = ""), adj = c(1.1, 1.5), col = adjustcolor(col2, alpha.f = 0.6), cex = 0.9)
          text(c[1], 0, labels = paste("(", c[1], ", 0)", sep = ""), adj = c(0.7, 1.2), col = adjustcolor("black", alpha.f = 0.6), cex = 0.9)
        }
        if (is.null(agents_names)) {
          text(1.18*c[1], 0.01 * c[2], labels = expression(x[1]), pos = 1, col = adjustcolor("black", alpha.f = 0.4))
          text(0, 1.18*c[2], labels = expression(x[2]), pos = 2, col = adjustcolor("black", alpha.f = 0.4))}
        else {
          text(1.18*c[1], 0.01 * c[2], labels = agents[1], pos = 1, col = adjustcolor("black", alpha.f = 0.4))
          text(0, 1.18*c[2], labels = agents[2], pos = 2, col = adjustcolor("black", alpha.f = 0.4))
        }
      }
    }
  }

  # Caso n=3
  else if (n == 3) {

    # Proyectado y 3D
    if (dimension == "2D" && representation == "projection") {

      # Caso 1: El primer y segundo coste (en orden creciente) son iguales y distintos de 0
      if (c[1] != 0 & c[1] == c[2]) {
        if (labels == TRUE) {
          plot(c[1], c[2], type="n", xlim = c(-0.2 * c[1], 1.2 * c[1]), ylim = c(0, 1.2 * c[2]),
               xlab = "", ylab = "",
               main = paste("Projected NS set for c = (", paste(c, collapse = ", "), ")", sep = ""),
               axes = FALSE, frame.plot = FALSE)}
        else {
          plot(c[1], c[2], type="n", xlim = c(-0.2 * c[1], 1.2 * c[1]), ylim = c(0, 1.2 * c[2]),
               xlab = "", ylab = "", axes = FALSE, frame.plot = FALSE)
        }
        # Coloración del NS set
        polygon(x = c(0, c[1], c[1], 0), y = c(0, 0, c[2] - c[1], c[2]), col = adjustcolor(col, alpha.f = 0.6), border = NA)

        # Líneas que lo conforman
        lines(c(0, 1.2 * c[1]), c(0, 0), lty = "longdash", lwd = 1.6, col = adjustcolor("black", alpha.f = 0.4))
        lines(c(0, 0), c(0, 1.2 * c[2]), lty = "longdash", lwd = 1.6, col = adjustcolor("black", alpha.f = 0.4))
        lines(c(0, c[1]), c(0, 0), lwd = 3, col = adjustcolor(col1, alpha.f = 0.6))
        lines(c(0, 0), c(0, c[2]), lwd = 3, col = adjustcolor(col1, alpha.f = 0.6))
        lines(c(0, c[1]), c(c[2], c[2] - c[1]), col = adjustcolor(col, alpha.f = 0.6), lwd = 3)
        if (labels == TRUE) {
          lines(c(0, c[1]), c(c[2], c[2]), lwd = 1.6, lty = "dashed", col = adjustcolor("grey15", alpha.f = 0.5))
          lines(c(c[2], c[1]), c(0, c[2]), lwd = 1.6, lty = "dashed", col = adjustcolor("grey15", alpha.f = 0.5))}

        # Puntos que delimitan el NS check
        points(0, c[2], col = adjustcolor(col1, alpha.f = 0.6), pch = 20)
        points(c[1], 0, col = adjustcolor(col1, alpha.f = 0.6), pch = 20)
        points(0, 0, col = adjustcolor(col1, alpha.f = 0.6), pch = 20)
        for (j in 1:length(contributions)) {
          points(contributions[[j]][1], contributions[[j]][2], col = colors[j %% length(colors)], pch = 19, cex = 1.2)
          if (labels == TRUE) {
            label_text <- if (!is.null(contributions_names)) {
              contributions_names[j]
            } else {
              paste0("(", contributions[[j]][1], ", ", contributions[[j]][2], ")")
            }
            text(contributions[[j]][1], contributions[[j]][2],
                 labels = label_text, adj = c(-0.2, -0.3),
                 col = colors[j %% length(colors)], font = 2, cex = 1.1)
          }
        }

        # Coordenadas específicas de los ejes
        if (labels == TRUE) {
          text(0, 0, labels = "(0, 0)", adj = c(1.1,1.1), col = adjustcolor(col2, alpha.f = 0.6), cex = 0.9)
          text(c[1], 0, labels = paste("(", c[1], ", 0)", sep = ""), adj = c(0.7, 1.2), col = adjustcolor(col2, alpha.f = 0.6), cex = 0.9)
          text(0, c[2], labels = paste("(0, ", c[2], ")", sep = ""), adj=c(1.1,1.1), col = adjustcolor(col2, alpha.f = 0.6), cex = 0.9)}
        if (is.null(agents_names)) {
          text(1.18*c[1], 0.01*c[2], labels = expression(x[1]), pos = 1, col = adjustcolor("black", alpha.f = 0.4))
          text(0, 1.18 * c[2], labels = expression(x[2]), pos = 2, col = adjustcolor("black", alpha.f = 0.4))}
        else {
          text(1.18*c[1], 0.01*c[2], labels = agents[1], pos = 1, col = adjustcolor("black", alpha.f = 0.4))
          text(0, 1.18 * c[2], labels = agents[2], pos = 2, col = adjustcolor("black", alpha.f = 0.4))
        }
      }

      # Caso 2: El primer coste es nulo
      else if (c[1] == 0 & c[2] != 0) {
        # Se dibuja el gráfico con los límites ajustados
        if (labels == TRUE) {
          plot(c[1], c[2], type="n", xlim = c(-0.2 * c[2], 0.65 * c[2]), ylim = c(0, 1.2 * c[2]),
               xlab = "", ylab = "",
               main = paste("Projected NS set for c = (", paste(c, collapse = ", "), ")", sep = ""),
               axes = FALSE, frame.plot = FALSE)}
        else {
          plot(c[1], c[2], type="n", xlim = c(-0.2 * c[2], 0.65 * c[2]), ylim = c(0, 1.2 * c[2]),
               xlab = "", ylab = "", axes = FALSE, frame.plot = FALSE)
        }

        # Líneas que lo conforman
        lines(c(0, 0.65 * c[2]), c(0, 0), lty = "dashed", lwd = 1.6, col = adjustcolor("black", alpha.f = 0.4))
        lines(c(0, 0), c(0, 1.2 * c[2]), lty = "longdash", lwd = 1.6, col = adjustcolor("black", alpha.f = 0.4))
        lines(c(0, 0), c(0, c[2]), lwd = 3, col = adjustcolor(col1, alpha.f = 0.6))

        # Puntos que delimita el NS check
        points(0, c[2], col = adjustcolor(col1, alpha.f = 0.6), pch = 20)
        points(0, 0, col = adjustcolor(col1, alpha.f = 0.6), pch = 20)
        for (j in 1:length(contributions)) {
          points(contributions[[j]][1], contributions[[j]][2], col = colors[j %% length(colors)], pch = 19, cex = 1.2)
          if (labels == TRUE) {
            label_text <- if (!is.null(contributions_names)) {
              contributions_names[j]
            } else {
              paste0("(", contributions[[j]][1], ", ", contributions[[j]][2], ")")
            }
            text(contributions[[j]][1], contributions[[j]][2],
                 labels = label_text, adj = c(-0.2, -0.3),
                 col = colors[j %% length(colors)], font = 2, cex = 1.1)
          }
        }

        # Coordenadas específicas de los ejes
        if (labels == TRUE) {
          text(0, 0, labels = "(0, 0)", adj = c(1.1,1.1), col = adjustcolor(col2, alpha.f = 0.6), cex = 0.9)
          text(0, c[2], labels = paste("(0, ", c[2], ")", sep = ""), adj = c(1.1, 1), col = adjustcolor(col2, alpha.f = 0.6), cex = 0.9)}
        if (is.null(agents_names)) {
          text(0.64*c[2], 0.01*c[2], labels = expression(x[1]), pos = 1, col = adjustcolor("black", alpha.f = 0.4))
          text(0, 1.18*c[2], labels = expression(x[2]), pos = 2, col = adjustcolor("black", alpha.f = 0.4))}
        else {
          text(0.64*c[2], 0.01*c[2], labels = agents[1], pos = 1, col = adjustcolor("black", alpha.f = 0.6))
          text(0, 1.18*c[2], labels = agents[2], pos = 2, col = adjustcolor("black", alpha.f = 0.6))
        }
      }

      # Caso 3: Los dos primeros costes son nulos
      else if (c[1] == 0 & c[2] == 0) {
        # Se dibuja el gráfico con los límites ajustados
        if (labels == TRUE) {
          plot(c[1], c[2], type="n", xlab = "", ylab = "", xlim = c(-0.05,0.5), ylim = c(0,0.5),
               main = paste("Projected NS set for c = (", paste(c, collapse = ", "), ")", sep = ""),
               axes = FALSE, frame.plot = FALSE)}
        else {
          plot(c[1], c[2], type="n", xlab = "", ylab = "", xlim = c(-0.05,0.5), ylim = c(0,0.5),
               axes = FALSE, frame.plot = FALSE)
        }
        # Líneas que lo conforman
        lines(c(0, 0.5), c(0, 0), lty = "longdash", lwd = 1.6, col = adjustcolor("black", alpha.f = 0.4))
        lines(c(0, 0), c(0, 0.5), lty = "longdash", lwd = 1.6, col = adjustcolor("black", alpha.f = 0.4))

        # Punto que delimita el NS check
        points(0, 0, col = adjustcolor(col1, alpha.f = 0.6), pch = 19)
        for (j in 1:length(contributions)) {
          points(contributions[[j]][1], contributions[[j]][2], col = colors[j %% length(colors)], pch = 19, cex = 1.2)
          if (labels == TRUE) {
            label_text <- if (!is.null(contributions_names)) {
              contributions_names[j]
            } else {
              paste0("(", contributions[[j]][1], ", ", contributions[[j]][2], ")")
            }
            text(contributions[[j]][1], contributions[[j]][2],
                 labels = label_text, adj = c(-0.2, -0.3),
                 col = colors[j %% length(colors)], font = 2, cex = 1.1)
          }
        }

        # Coordenadas específicas de los ejes
        if (is.null(agents_names)) {
          text(0.49, 0, labels = expression(x[1]), adj=c(0.6,1.3), col = adjustcolor("black", alpha.f = 0.4))
          text(0, 0.49, labels = expression(x[2]), pos = 2, col = adjustcolor("black", alpha.f = 0.4))}
        else {
          text(0.48, 0, labels = agents[1], adj=c(0.6,1.3), col = adjustcolor("black", alpha.f = 0.4))
          text(0, 0.48, labels = agents[2], pos = 2, col = adjustcolor("black", alpha.f = 0.4))
        }
      }

      # Caso 4: Los dos costes son positivos y diferentes entre si
      else {
        # Se dibuja el gráfico con los límites ajustados
        if (labels == TRUE) {
          plot(c[1], c[2], type="n", xlim = c(-0.2 * c[1], 1.2 * c[1]), ylim = c(0, 1.2 * c[2]),
               xlab = "", ylab = "",
               main = paste("Projected NS set for c = (", paste(c, collapse = ", "), ")", sep = ""),
               axes = FALSE, frame.plot = FALSE)}
        else {
          plot(c[1], c[2], type="n", xlim = c(-0.2 * c[1], 1.2 * c[1]), ylim = c(0, 1.2 * c[2]),
               xlab = "", ylab = "", axes = FALSE, frame.plot = FALSE)
        }

        # Coloración del NS set
        polygon(x = c(0, c[1], c[1], 0, 0), y = c(0, 0, c[2] - c[1], c[2], 0), col = adjustcolor(col, alpha.f = 0.6), border = NA)

        # Líneas que lo conforman
        lines(c(0, 1.2 * c[1]), c(0, 0), lty = "longdash", lwd = 1.6, col = adjustcolor("black", alpha.f = 0.4))
        lines(c(0, 0), c(0, 1.2* c[2]), lty = "longdash", lwd = 1.6, col = adjustcolor("black", alpha.f = 0.4))
        lines(c(0, c[1]), c(0, 0), lwd = 3, col = adjustcolor(col1, alpha.f = 0.6))
        lines(c(0, 0), c(0, c[2]), lwd = 3, col = adjustcolor(col1, alpha.f = 0.6))
        lines(c(c[1], c[1]), c(c[2] - c[1], 0), lwd = 3, col = adjustcolor(col1, alpha.f = 0.6))
        lines(c(0, c[1]), c(c[2], c[2] - c[1]), lwd = 3, col = adjustcolor(col1, alpha.f = 0.6))
        if (labels == TRUE) {
          lines(c(0, c[1]), c(c[2], c[2]), lwd = 1.6, lty = "dashed", col = adjustcolor("grey15", alpha.f = 0.5))
          lines(c(c[1], c[1]), c(c[2] - c[1], c[2]), lwd = 1.6, lty = "dashed", col = adjustcolor("grey15", alpha.f = 0.5))}

        # Puntos que delimitan el NS check
        points(0, 0, col = adjustcolor(col1, alpha.f = 0.6), pch = 20)
        points(c[1], 0, col = adjustcolor(col1, alpha.f = 0.6), pch = 20)
        points(0, c[2], col = adjustcolor(col1, alpha.f = 0.6), pch = 20)
        points(c[1], c[2] - c[1], col = adjustcolor(col1, alpha.f = 0.6), pch = 20)
        for (j in 1:length(contributions)) {
          points(contributions[[j]][1], contributions[[j]][2], col = colors[j %% length(colors)], pch = 19, cex = 1.2)
          if (labels == TRUE) {
            label_text <- if (!is.null(contributions_names)) {
              contributions_names[j]
            } else {
              paste0("(", contributions[[j]][1], ", ", contributions[[j]][2], ")")
            }
            text(contributions[[j]][1], contributions[[j]][2],
                 labels = label_text, adj = c(-0.2, -0.3),
                 col = colors[j %% length(colors)], font = 2, cex = 1.1)
          }
        }

        # Coordenadas específicas de los ejes
        if (labels == TRUE) {
          text(0, 0, labels = "(0, 0)", adj = c(1.1,1.1), col = adjustcolor(col2, alpha.f = 0.6), cex = 0.9)
          text(c[1], 0, labels = paste("(", c[1], ", 0)", sep = ""), adj = c(0.7, 1.2), col = adjustcolor(col2, alpha.f = 0.6), cex = 0.9)
          text(0, c[2], labels = paste("(0, ", c[2], ")", sep = ""), adj=c(1.1,1.1), col = adjustcolor(col2, alpha.f = 0.6), cex = 0.9)
          text(c[1], c[2] - c[1], labels = paste("(", c[1], ", ", c[2] - c[1], ")", sep = ""), pos = 1, col = adjustcolor(col2, alpha.f = 0.6), cex = 0.9)}
        if (is.null(agents_names)) {
          text(1.18*c[1], 0.01 * c[2], labels = expression(x[1]), pos = 1, col = adjustcolor("black", alpha.f = 0.4))
          text(0, 1.18*c[2], labels = expression(x[2]), pos = 2, col = adjustcolor("black", alpha.f = 0.4))}
        else {
          text(1.18*c[1], 0.01 * c[2], labels = agents[1], pos = 1, col = adjustcolor("black", alpha.f = 0.6))
          text(0, 1.18*c[2], labels = agents[2], pos = 2, col = adjustcolor("black", alpha.f = 0.6))
        }
      }
    }

    # Real y 2D
    if (dimension == "2D" && representation == "real") {
      if (c[1] == 0 & c[2] == 0) {
        if (labels == TRUE) {
          plot(c(0,1), c(0, -1.732051), type = "l", xlab = "", ylab = "", axes = FALSE, lwd = 1.6,
               main = paste("Real NS set for c = (", paste(c, collapse = ", "), ")", sep = ""), lty = "dashed", col = adjustcolor("black", alpha.f = 0.4))}
        else {
          plot(c(0,1), c(0, -1.732051), type = "l", xlab = "", ylab = "", axes = FALSE, lwd = 1.6, lty = "dashed", col = adjustcolor("black", alpha.f = 0.4))
        }
        # Punto que delimita el NS check
        points(0, 0, col = adjustcolor(col1, alpha.f = 0.6), pch = 20)
        # Coordenadas específicas de los ejes
        if (labels == TRUE) {
          text(0, 0, labels = "(0, 0, 0)", adj = c(-0.2,-0.2), col = adjustcolor(col2, alpha.f = 0.6), cex = 1)
        }

        for (j in 1:length(contributions)) {
          points(0, 0, col = colors[j %% length(colors)], pch = 19, cex = 1.2)  # Todos en el mismo punto
          if (labels == TRUE) {
            label_text <- if (!is.null(contributions_names)) {
              contributions_names[j]
            } else {
              paste0("(", contributions[[j]][1], ", ", contributions[[j]][2], ", ", contributions[[j]][3], ")")
            }
            text(0, 0, labels = label_text, pos = 1,
                 col = colors[j %% length(colors)], font = 2, cex = 1.1)
          }
        }
      } else {
        # Se definen los vértices del conjunto de imputaciones
        II <- matrix(c(c[1], -c[1], c[3],
                       -c[2], c[2], c[3],
                       c[1], c[2], c[3] - c[1] - c[2]), nrow = 3, byrow = TRUE)

        # Se determina lo longitud del lado del triángulo de imputaciones
        ll <- c[1] + c[2]

        # Se establecen los vértices del triángulo equilátero y su respectiva matriz de proyección
        Equilatero <- matrix(c(0, ll, ll / 2, 0,
                               0, 0, -sqrt(3) / 2 * ll, 0), nrow = 2, byrow = TRUE)

        M <- Equilatero[, 1:3] %*% solve(t(II))

        # Construcción del rectángulo
        rectangulo <- matrix(c(0, 0, c[3],
                               c[1], 0, c[3] - c[1],
                               c[1], c[2], c[3] - c[1] - c[2],
                               0, c[2], c[3] - c[2]), nrow = 4, byrow = TRUE)
        PuntosRectangulo <- M %*% t(rectangulo)
        PuntosRectangulo <- cbind(PuntosRectangulo, PuntosRectangulo[, 1])

        # Vectores de contribuciones
        contributions_matrix <- do.call(rbind, contributions)
        contributions_points <- M %*% t(contributions_matrix)

        extend_range <- function(vec, factor = 0.1) {
          range(vec) + c(-1, 1) * factor * diff(range(vec))
        }
        # Calcula los límites considerando TODOS los puntos
        x_lim <- extend_range(c(PuntosRectangulo[1, ], contributions_points[1, ]))
        y_lim <- extend_range(c(PuntosRectangulo[2, ], contributions_points[2, ]))

        if (labels == TRUE) {
          plot(PuntosRectangulo[1, ], PuntosRectangulo[2, ], type = "l", col = col1, xlab = "", ylab = "", axes = FALSE, lwd = 3,
               main = paste("Real NS set for c = (", paste(c, collapse = ", "), ")", sep = ""), xlim = x_lim, ylim = y_lim)
        } else {
          plot(PuntosRectangulo[1, ], PuntosRectangulo[2, ], type = "l", col = col1, xlab = "", ylab = "", axes = FALSE, lwd = 3)
        }

        # NS set
        vertices <- unique(matrix(c(0, 0, c[3],
                                    c[1], 0, c[3] - c[1],
                                    c[1], c[2] - c[1], c[3] - c[2],
                                    0, c[2], c[3] - c[2]), nrow = 4, byrow = TRUE))

        vertices <- rbind(vertices, vertices[1, ])
        puntos <- M %*% t(vertices)

        # Dibujar el área del conjunto NS
        polygon(puntos[1, ], puntos[2, ], col = adjustcolor(col, alpha.f = 0.6), border = adjustcolor(col1, alpha.f = 0.6), lwd = 2)

        # Puntos que delimita el NS check
        points(puntos[1, ], puntos[2, ], col = adjustcolor(col1, alpha.f = 0.6), pch = 20)
        points(contributions_points[1, ], contributions_points[2, ], col = colors, pch = 19, cex = 1.3)
        for (j in 1:ncol(contributions_points)) {
          if (labels == TRUE) {
            label_text <- if (!is.null(contributions_names)) {
              contributions_names[j]
            } else {
              paste0("(", contributions[[j]][1], ", ", contributions[[j]][2], ", ", contributions[[j]][3], ")")
            }
            text(contributions_points[1, j], contributions_points[2, j],
                 labels = label_text, pos = 3,
                 col = colors[j %% length(colors)], font = 2, cex = 1.1)
          }
        }

        # Coordenadas específicas de los ejes
        if (labels == TRUE) {
          text(puntos[1], puntos[2],
               labels = paste0("(", vertices[1,1], ", ", vertices[1,2], ", ", vertices[1,3], ")"),
               col = adjustcolor(col2, alpha.f = 0.6), pos = 4, cex = 1)
          if (c[1] == 0 & c[2] != 0) {
            text(puntos[1,2], puntos[2,2],
                 labels = paste0("(", vertices[2,1], ", ", vertices[2,2], ", ", vertices[1,3], ")"),
                 col = adjustcolor(col2, alpha.f = 0.6), pos = 2, cex = 1)
          } else {
            text(puntos[1,2], puntos[2,2],
                 labels = paste0("(", vertices[2,1], ", ", vertices[2,2], ", ", vertices[2,3], ")"),
                 col = adjustcolor(col2, alpha.f = 0.6), pos = 2, cex = 1)
            if (c[1] != 0 & c[1] == c[2]) {
              text(puntos[1,3], puntos[2,3],
                   labels = paste0("(", vertices[3,1], ", ", vertices[3,2], ", ", vertices[3,3], ")"),
                   col = adjustcolor(col2, alpha.f = 0.6), pos = 4, cex = 1)
            } else {
              text(puntos[1,3], puntos[2,3],
                   labels = paste0("(", vertices[3,1], ", ", vertices[3,2], ", ", vertices[3,3], ")"),
                   col = adjustcolor(col2, alpha.f = 0.6), pos = 2, cex = 1)
              text(puntos[1,4], puntos[2,4],
                   labels = paste0("(", vertices[4,1], ", ", vertices[4,2], ", ", vertices[4,3], ")"),
                   col = adjustcolor(col2, alpha.f = 0.6), pos = 4, cex = 1)
            }
          }
        }
      }
    }
    # Real y 3D
    else if (dimension == "3D" && representation == "real") {

      # Se definen los vértices del conjunto NS
      vertices <- matrix(c(
        0, 0, c[3],
        c[1], 0, c[3] - c[1],
        c[1], c[2] - c[1], c[3] - c[2],
        0, c[2], c[3] - c[2]
      ), nrow = 4, byrow = TRUE)

      # Se agrega el primer punto nuevamente para cerrar la figura
      vertices <- rbind(vertices, vertices[1,])

      # Se definen las caras del polígono en 3D
      faces <- list(
        c(1, 2, 3),
        c(1, 3, 4)
      )

      # Se establecen las etiquetas
      vertex_labels <- apply(vertices[1:4, ], 1, function(v) {
        sprintf("(%g, %g, %g)", v[1], v[2], v[3])
      })

      # Se definen los puntos de la lista 'contributions'
      contributions_x <- sapply(contributions, function(contribution) contribution[1])
      contributions_y <- sapply(contributions, function(contribution) contribution[2])
      contributions_z <- sapply(contributions, function(contribution) contribution[3])

      # Se establecen las etiquetas de la lista 'contributions'
      if (is.null(contributions_names)) {
        contributions_labels <- sapply(contributions, function(c) {
          sprintf("(%g, %g, %g)", c[1], c[2], c[3])
        })
      } else {
        contributions_labels <- contributions_names
      }

      # Definimos el rótulo de los ejes
      if (!is.null(agents_names) ) {
        x_title <- agents[1]
        y_title <- agents[2]
        z_title <- agents[3]
      } else {
        x_title <- "x<sub>1</sub>"
        y_title <- "x<sub>2</sub>"
        z_title <- "x<sub>3</sub>"
      }

      # Construcción de la figura
      fig <- plot_ly() %>%
        # Dibujo del rectángulo base
        add_trace(
          type = "scatter3d",
          mode = "lines",
          x = c(0, c[1], c[1], 0, 0),
          y = c(0, 0, c[2], c[2], 0),
          z = c(c[3], c[3] - c[1], c[3] - c[1] - c[2], c[3] - c[2], c[3]),
          line = list(color = adjustcolor(col1, alpha.f = 0.8), width = 5),
          showlegend = FALSE,
          hoverinfo = "none"
        ) %>%
        # Dibujo del polígono NS
        add_trace(
          type = "mesh3d",
          x = vertices[, 1],
          y = vertices[, 2],
          z = vertices[, 3],
          i = unlist(lapply(faces, function(x) x[1] - 1)),
          j = unlist(lapply(faces, function(x) x[2] - 1)),
          k = unlist(lapply(faces, function(x) x[3] - 1)),
          facecolor = c(col, col),
          opacity = 0.45,
          showlegend = FALSE,
          hoverinfo = "none"
        )

      # Se establecen las etiquetas (solo si labels == TRUE)
      if (is.logical(labels) && labels) {
        fig <- fig %>%
          add_trace(
            type = "scatter3d",
            mode = "text",
            x = vertices[1:4, 1],
            y = vertices[1:4, 2],
            z = vertices[1:4, 3],
            text = paste0("<b>", vertex_labels, "</b>"),
            textposition = "bottom center",
            textfont = list(size = 12, color = adjustcolor(col2, alpha.f = 0.8)),
            hoverinfo = "none"
          )
      }

      # Agregar puntos en los vértices
      fig <- fig %>%
        add_trace(
          type = "scatter3d",
          mode = "markers",
          x = vertices[1:4, 1],
          y = vertices[1:4, 2],
          z = vertices[1:4, 3],
          marker = list(size = 2.5, color = adjustcolor(col1, alpha.f = 0.8), symbol = "circle"),
          hoverinfo = "none",
          showlegend = FALSE
        )

      fig <- fig %>%
        add_trace(
          type = "scatter3d",
          mode = "markers",
          x = contributions_x,
          y = contributions_y,
          z = contributions_z,
          marker = list(size = 5, color = colors, symbol = "circle"),
          hoverinfo = "none",
          showlegend = FALSE
        )

      if (labels == TRUE) {
        fig <- fig %>%
          add_trace(
            type = "scatter3d",
            mode = "text",
            x = contributions_x,
            y = contributions_y,
            z = contributions_z ,
            text = paste0("<b>", contributions_labels, "</b>"),
            textfont = list(size = 14, color = colors),
            hoverinfo = "none",
            showlegend = FALSE
          )
      }

      # Configurar la vista y los ejes (solo incluir el título si labels == TRUE)
      if (is.logical(labels) && labels) {
        fig <- fig %>%
          layout(
            title = list(
              text = paste("<b>Allocations for c = (", paste(c, collapse = ", "), ")", sep = ""),
              font = list(
                family = "arial",
                size = 19,
                color = "black"
              ),
              y = 0.99
            ),
            scene = list(
              xaxis = list(title = x_title, range = c(0, NULL)),
              yaxis = list(title = y_title, range = c(0, NULL)),
              zaxis = list(title = z_title, range = c(0, 1.2*max(vertices[,3]))),
              camera = list(eye = list(x = 1.7, y = 1.7, z = 1.7))
            )
          )
      } else {
        fig <- fig %>%
          layout(
            scene = list(
              xaxis = list(title = x_title, range = c(0, NULL)),
              yaxis = list(title = y_title, range = c(0, NULL)),
              zaxis = list(title = z_title, range = c(0, 1.2*max(vertices[,3]))),
              camera = list(eye = list(x = 1.7, y = 1.7, z = 1.7))
            )
          )
      }

      # Se muestra la figura
      print(fig)
    }

    # Ambos y 3D
    else if (dimension == "3D" && representation == "both") {

      # Se definen los vértices del conjunto NS en 3D
      vertices <- matrix(c(
        0, 0, c[3],
        c[1], 0, c[3] - c[1],
        c[1], c[2] - c[1], c[3] - c[2],
        0, c[2], c[3] - c[2]
      ), nrow = 4, byrow = TRUE)

      # Se agrega el primer punto nuevamente para cerrar la figura
      vertices <- rbind(vertices, vertices[1,])

      # Se definen las caras del polígono en 3D
      faces <- list(
        c(1, 2, 3),
        c(1, 3, 4)
      )

      # Se define la proyección en el plano z
      vertices_proj <- vertices
      vertices_proj[, 3] <- 0

      # Se establecen las etiquetas
      vertex_labels <- apply(vertices[1:4, ], 1, function(v) {
        sprintf("(%g, %g, %g)", v[1], v[2], v[3])
      })

      # Se definen los puntos de la lista 'contributions'
      contributions_x <- sapply(contributions, function(contribution) contribution[1])
      contributions_y <- sapply(contributions, function(contribution) contribution[2])
      contributions_z <- sapply(contributions, function(contribution) contribution[3])

      # Se establecen las etiquetas de la lista 'contributions'
      if (is.null(contributions_names)) {
        contributions_labels <- sapply(contributions, function(c) {
          sprintf("(%g, %g, %g)", c[1], c[2], c[3])
        })
      } else{
        contributions_labels <- contributions_names
      }

      # Se definen los puntos proyectados en el eje Z de 'contributions'
      # Proyección de los puntos de 'contributions' en el plano Z
      contributions_proj <- sapply(contributions, function(contribution) {
        # Se crea la proyección estableciendo la componente Z a 0
        contribution_proj <- contribution
        contribution_proj[3] <- 0  # Establecemos Z = 0 para la proyección
        return(contribution_proj)
      })

      # Definimos el rótulo de los ejes
      if (!is.null(agents_names) ) {
        x_title <- agents[1]
        y_title <- agents[2]
        z_title <- agents[3]
      } else {
        x_title <- "x<sub>1</sub>"
        y_title <- "x<sub>2</sub>"
        z_title <- "x<sub>3</sub>"
      }

      # El primer y segundo coste son nulos y el otro es positivo
      if (c[1] == 0 && c[2] == 0 && c[3] > 0) {

        # Construcción de la figura
        fig <- plot_ly() %>%
          # Dibujo del rectángulo base
          add_trace(
            type = "scatter3d",
            mode = "lines",
            x = c(0, c[1], c[1], 0, 0),
            y = c(0, 0, c[2], c[2], 0),
            z = c(c[3], c[3] - c[1], c[3] - c[1] - c[2], c[3] - c[2], c[3]),
            line = list(color = adjustcolor(col1, alpha.f = 0.8), width = 5),
            showlegend = FALSE,
            hoverinfo = "none"
          ) %>%

          # Se agrega la proyección en el plano z
          add_trace(
            type = "scatter3d",
            mode = "markers",
            x = vertices_proj[, 1],
            y = vertices_proj[, 2],
            z = vertices_proj[, 3],
            marker = list(size = 2.5, color = adjustcolor(col3, alpha.f = 0.8), symbol = "circle"),
            opacity = 0.2,
            showlegend = FALSE,
            hoverinfo = "none"
          )

        # Se establecen las etiquetas
        if (is.logical(labels) && labels) {
          fig <- fig %>%
            add_trace(
              type = "scatter3d",
              mode = "text",
              x = vertices[1:4, 1],
              y = vertices[1:4, 2],
              z = vertices[1:4, 3],
              text = paste0("<b>", vertex_labels, "</b>"),
              textposition = "bottom center",
              textfont = list(size = 12, color = adjustcolor(col2, alpha.f = 0.8)),
              hoverinfo = "none"
            )
        }

        # Se agregan los puntos de los vértices
        fig <- fig %>%
          add_trace(
            type = "scatter3d",
            mode = "markers",
            x = vertices[1:4, 1],
            y = vertices[1:4, 2],
            z = vertices[1:4, 3],
            marker = list(size = 2.5, color = adjustcolor(col1, alpha.f = 0.8), symbol = "circle"),
            hoverinfo = "none",
            showlegend = FALSE
          )

        fig <- fig %>%
          add_trace(
            type = "scatter3d",
            mode = "markers",
            x = contributions_x,
            y = contributions_y,
            z = contributions_z,
            marker = list(size = 5, color = colors, symbol = "circle"),
            hoverinfo = "none",
            showlegend = FALSE
          )

        fig <- fig %>%
          add_trace(
            type = "scatter3d",
            mode = "markers",
            x = contributions_proj[1, ],
            y = contributions_proj[2, ],
            z = contributions_proj[3, ],
            marker = list(size = 5, color = shadow_colors, symbol = "circle"),
            hoverinfo = "none",
            showlegend = FALSE
          )

        if (labels == TRUE) {
          fig <- fig %>%
            add_trace(
              type = "scatter3d",
              mode = "text",
              x = contributions_x,
              y = contributions_y,
              z = contributions_z ,
              text = paste0("<b>", contributions_labels, "</b>"),
              textfont = list(size = 14, color = colors),
              hoverinfo = "none",
              showlegend = FALSE
            )
        }

        # Se configura la vista y los ejes
        if (is.logical(labels) && labels) {
          fig <- fig %>%
            layout(
              title = list(
                text = paste("<b>Allocations for c = (", paste(c, collapse = ", "), ")</b>", sep = ""),
                font = list(
                  family = "arial",
                  size = 19,
                  color = "black"
                ),
                y = 0.99
              ),
              scene = list(
                xaxis = list(title = x_title, range = c(0, NULL)),
                yaxis = list(title = y_title, range = c(0, NULL)),
                zaxis = list(title = z_title, range = c(0, 1.2*max(vertices[,3]))),
                camera = list(eye = list(x = 1.7, y = 1.7, z = 1.4))
              )
            )
        } else {
          fig <- fig %>%
            layout(
              scene = list(
                xaxis = list(title = x_title, range = c(0, NULL)),
                yaxis = list(title = y_title, range = c(0, NULL)),
                zaxis = list(title = z_title, range = c(0, 1.2*max(vertices[,3]))),
                camera = list(eye = list(x = 1.7, y = 1.7, z = 1.4))
              )
            )
        }
      }

      # El primer es coste es nulo y los otros dos positivos
      else if (c[1] == 0 && c[2] > 0 && c[3] > 0) {

        # Construcción de la figura
        fig <- plot_ly() %>%
          # Dibujo del rectángulo base
          add_trace(
            type = "scatter3d",
            mode = "lines",
            x = c(0, c[1], c[1], 0, 0),
            y = c(0, 0, c[2], c[2], 0),
            z = c(c[3], c[3] - c[1], c[3] - c[1] - c[2], c[3] - c[2], c[3]),
            line = list(color = adjustcolor(col1, alpha.f = 0.8), width = 5),
            showlegend = FALSE,
            hoverinfo = "none"
          ) %>%

          # Se agrega la proyección en el plano z
          add_trace(
            type = "scatter3d",
            mode = "lines",
            x = vertices_proj[, 1],
            y = vertices_proj[, 2],
            z = vertices_proj[, 3],
            line = list(color = adjustcolor(col3, alpha.f = 0.8), width = 5),
            opacity = 0.2,
            showlegend = FALSE,
            hoverinfo = "none"
          )

        # Se establecen las etiquetas
        if (is.logical(labels) && labels) {
          fig <- fig %>%
            add_trace(
              type = "scatter3d",
              mode = "text",
              x = vertices[1:4, 1],
              y = vertices[1:4, 2],
              z = vertices[1:4, 3],
              text = paste0("<b>", vertex_labels, "</b>"),
              textposition = "bottom center",
              textfont = list(size = 12, color = adjustcolor(col2, alpha.f = 0.8)),
              hoverinfo = "none"
            )
        }

        # Se agregan los puntos de los vértices
        fig <- fig %>%
          add_trace(
            type = "scatter3d",
            mode = "markers",
            x = vertices[1:4, 1],
            y = vertices[1:4, 2],
            z = vertices[1:4, 3],
            marker = list(size = 2.5, color = adjustcolor(col1, alpha.f = 0.8), symbol = "circle"),
            hoverinfo = "none",
            showlegend = FALSE
          )

        fig <- fig %>%
          add_trace(
            type = "scatter3d",
            mode = "markers",
            x = contributions_x,
            y = contributions_y,
            z = contributions_z,
            marker = list(size = 5, color = colors, symbol = "circle"),
            hoverinfo = "none",
            showlegend = FALSE
          )

        fig <- fig %>%
          add_trace(
            type = "scatter3d",
            mode = "markers",
            x = contributions_proj[1, ],
            y = contributions_proj[2, ],
            z = contributions_proj[3, ],
            marker = list(size = 5, color = shadow_colors, symbol = "circle"),
            hoverinfo = "none",
            showlegend = FALSE
          )

        if (labels == TRUE) {
          fig <- fig %>%
            add_trace(
              type = "scatter3d",
              mode = "text",
              x = contributions_x,
              y = contributions_y,
              z = contributions_z ,
              text = paste0("<b>", contributions_labels, "</b>"),
              textfont = list(size = 14, color = colors),
              hoverinfo = "none",
              showlegend = FALSE
            )
        }

        # Se configura la vista y los ejes
        if (is.logical(labels) && labels) {
          fig <- fig %>%
            layout(
              title = list(
                text = paste("<b>Allocations for c = (", paste(c, collapse = ", "), ")</b>", sep = ""),
                font = list(
                  family = "arial",
                  size = 19,
                  color = "black"
                ),
                y = 0.99
              ),
              scene = list(
                xaxis = list(title = x_title, range = c(0, NULL)),
                yaxis = list(title = y_title, range = c(0, NULL)),
                zaxis = list(title = z_title, range = c(0, 1.2*max(vertices[,3]))),
                camera = list(eye = list(x = 1.7, y = 1.7, z = 1.4))
              )
            )
        }
        else {
          fig <- fig %>%
            layout(
              scene = list(
                xaxis = list(title = x_title, range = c(0, NULL)),
                yaxis = list(title = y_title, range = c(0, NULL)),
                zaxis = list(title = z_title, range = c(0, 1.2*max(vertices[,3]))),
                camera = list(eye = list(x = 1.7, y = 1.7, z = 1.4))
              )
            )
        }

        # Resto de casos
      } else {

        # Construcción de la figura
        fig <- plot_ly() %>%
          # Dibujo del rectángulo base
          add_trace(
            type = "scatter3d",
            mode = "lines",
            x = c(0, c[1], c[1], 0, 0),
            y = c(0, 0, c[2], c[2], 0),
            z = c(c[3], c[3] - c[1], c[3] - c[1] - c[2], c[3] - c[2], c[3]),
            line = list(color = adjustcolor(col1, alpha.f = 0.8), width = 5),
            showlegend = FALSE,
            hoverinfo = "none"
          ) %>%

          # Se agrega la proyección en el plano z
          add_trace(
            type = "mesh3d",
            x = vertices_proj[, 1],
            y = vertices_proj[, 2],
            z = vertices_proj[, 3],
            i = unlist(lapply(faces, function(x) x[1] - 1)),
            j = unlist(lapply(faces, function(x) x[2] - 1)),
            k = unlist(lapply(faces, function(x) x[3] - 1)),
            facecolor = c(col3, col3),  # Different colors for projected faces
            opacity = 0.2,
            showlegend = FALSE,
            hoverinfo = "none"
          ) %>%

          # Dibujo del polígono NS
          add_trace(
            type = "mesh3d",
            x = vertices[, 1],
            y = vertices[, 2],
            z = vertices[, 3],
            i = unlist(lapply(faces, function(x) x[1] - 1)),
            j = unlist(lapply(faces, function(x) x[2] - 1)),
            k = unlist(lapply(faces, function(x) x[3] - 1)),
            facecolor = c(col, col),
            opacity = 0.45,
            showlegend = FALSE,
            hoverinfo = "none"
          )

        # Se establecen las etiquetas
        if (is.logical(labels) && labels) {
          fig <- fig %>%
            add_trace(
              type = "scatter3d",
              mode = "text",
              x = vertices[1:4, 1],
              y = vertices[1:4, 2],
              z = vertices[1:4, 3],
              text = paste0("<b>", vertex_labels, "</b>"),
              textposition = "bottom center",
              textfont = list(size = 12, color = adjustcolor(col2, alpha.f = 0.8)),
              hoverinfo = "none"
            )
        }

        # Se agregan puntos en los vértices
        fig <- fig %>%
          add_trace(
            type = "scatter3d",
            mode = "markers",
            x = vertices[1:4, 1],
            y = vertices[1:4, 2],
            z = vertices[1:4, 3],
            marker = list(size = 2.5, color = adjustcolor(col1, alpha.f = 0.8), symbol = "circle"),
            hoverinfo = "none",
            showlegend = FALSE
          )

        fig <- fig %>%
          add_trace(
            type = "scatter3d",
            mode = "markers",
            x = contributions_x,
            y = contributions_y,
            z = contributions_z,
            marker = list(size = 5, color = colors, symbol = "circle"),
            hoverinfo = "none",
            showlegend = FALSE
          )

        fig <- fig %>%
          add_trace(
            type = "scatter3d",
            mode = "markers",
            x = contributions_proj[1, ],
            y = contributions_proj[2, ],
            z = contributions_proj[3, ],
            marker = list(size = 5, color = shadow_colors, symbol = "circle"),
            hoverinfo = "none",
            showlegend = FALSE
          )

        if (labels == TRUE) {
          fig <- fig %>%
            add_trace(
              type = "scatter3d",
              mode = "text",
              x = contributions_x,
              y = contributions_y,
              z = contributions_z ,
              text = paste0("<b>", contributions_labels, "</b>"),
              textfont = list(size = 14, color = colors),
              hoverinfo = "none",
              showlegend = FALSE
            )
        }

        # Se configura la vista y los ejes
        if (is.logical(labels) && labels) {
          fig <- fig %>%
            layout(
              title = list(
                text = paste("<b>Allocations for c = (", paste(c, collapse = ", "), ")</b>", sep = ""),
                font = list(
                  family = "arial",
                  size = 19,
                  color = "black"
                ),
                y = 0.99
              ),
              scene = list(
                xaxis = list(title = x_title, range = c(0, NULL)),
                yaxis = list(title = y_title, range = c(0, NULL)),
                zaxis = list(title = z_title, range = c(0, 1.2*max(vertices[,3]))),
                camera = list(eye = list(x = 1.7, y = 1.7, z = 1.4))
              )
            )
        } else {
          fig <- fig %>%
            layout(
              scene = list(
                xaxis = list(title = x_title, range = c(0, NULL)),
                yaxis = list(title = y_title, range = c(0, NULL)),
                zaxis = list(title = z_title, range = c(0, 1.2*max(vertices[,3]))),
                camera = list(eye = list(x = 1.7, y = 1.7, z = 1.4))
              )
            )
        }
      }

      # Se muestra la figura
      print(fig)
    }
  }

  # Caso n=4
  else if (n == 4) {

    # Proyectado y 3D
    if (dimension == "3D" && representation == "projection") {

      # Se definen los vértices
      vertices <- matrix(c(
        0, 0, 0,
        c[1], 0, 0,
        c[1], c[2] - c[1], 0,
        0, c[2], 0,
        0, 0, c[3],
        c[1], 0, c[3] - c[1],
        c[1], c[2] - c[1], c[3] - c[2],
        0, c[2], c[3] - c[2]
      ), ncol = 3, byrow = TRUE)

      # Se definen las caras del poliedro
      caras <- list(
        c(1, 2, 3, 4),
        c(5, 6, 7, 8),
        c(2, 3, 7, 6),
        c(1, 4, 8, 5),
        c(1, 2, 6, 5),
        c(3, 4, 8, 7)
      )

      # Se establecen las etiquetas de los vértices
      vertex_labels <- apply(vertices[1:8, ], 1, function(v) {
        sprintf("(%g, %g, %g)", v[1], v[2], v[3])
      })

      # Se define el rótulo de los ejes
      if (!is.null(agents_names) ) {
        x_title <- agents[1]
        y_title <- agents[2]
        z_title <- agents[3]
      } else {
        x_title <- "x<sub>1</sub>"
        y_title <- "x<sub>2</sub>"
        z_title <- "x<sub>3</sub>"
      }

      # Índices para definir las caras
      i <- c()
      j <- c()
      k <- c()
      for (cara in caras) {
        i <- c(i, cara[1] - 1)
        j <- c(j, cara[2] - 1)
        k <- c(k, cara[3] - 1)
        i <- c(i, cara[1] - 1)
        j <- c(j, cara[3] - 1)
        k <- c(k, cara[4] - 1)
      }

      # Se definen los puntos de la lista 'contributions'
      contributions_x <- sapply(contributions, function(contribution) contribution[1])
      contributions_y <- sapply(contributions, function(contribution) contribution[2])
      contributions_z <- sapply(contributions, function(contribution) contribution[3])

      # Se establecen las etiquetas de la lista 'contributions'
      if (is.null(contributions_names)) {
        contributions_labels <- sapply(contributions, function(c) {
          sprintf("(%g, %g, %g)", c[1], c[2], c[3])
        })
      } else {
        contributions_labels <- contributions_names
      }

      # Construcción de la figura

      # Dibujo del poliedro NS
      fig <- plot_ly() %>%
        add_trace(
          type = "mesh3d",
          x = vertices[, 1],
          y = vertices[, 2],
          z = vertices[, 3],
          i = i,
          j = j,
          k = k,
          facecolor = c(rep(col3, 2), rep(col, 10)),
          opacity = 0.3,
          showlegend = FALSE,
          hoverinfo = "none"
        )

      # Dibujo de los bordes de las caras
      for (cara in caras) {
        cara <- c(cara, cara[1])
        fig <- fig %>%
          add_trace(
            type = "scatter3d", mode = "lines",
            x = vertices[cara, 1],
            y = vertices[cara, 2],
            z = vertices[cara, 3],
            line = list(color = adjustcolor(col1, alpha.f = 0.8), width =5),
            showlegend = FALSE,
            hoverinfo = "none"
          )
      }

      # Se establecen las etiquetas de los vértices
      if (is.logical(labels) && labels) {
        fig <- fig %>%
          add_trace(
            type = "scatter3d",
            mode = "text",
            x = vertices[, 1],
            y = vertices[, 2],
            z = vertices[, 3],
            text = paste0("<b>", vertex_labels, "</b>"),
            textposition = "bottom center",
            textfont = list(size = 12, color = adjustcolor(col2, alpha.f = 0.8)),
            hoverinfo = "none",
            showlegend = FALSE
          )}

      # Se agregan puntos en los vértices
      fig <- fig %>%
        add_trace(
          type = "scatter3d", mode = "markers",
          x = vertices[, 1],
          y = vertices[, 2],
          z = vertices[, 3],
          marker = list(size = 2.5, color = adjustcolor(col1, alpha.f = 0.8), symbol = "circle"),
          hoverinfo = "none",
          showlegend = FALSE
        )

      fig <- fig %>%
        add_trace(
          type = "scatter3d",
          mode = "markers",
          x = contributions_x,
          y = contributions_y,
          z = contributions_z,
          marker = list(size = 5, color = colors, symbol = "circle"),
          hoverinfo = "none",
          showlegend = FALSE
        )

      if (labels == TRUE) {
        fig <- fig %>%
          add_trace(
            type = "scatter3d",
            mode = "text",
            x = contributions_x,
            y = contributions_y,
            z = contributions_z ,
            text = paste0("<b>", contributions_labels, "</b>"),
            textfont = list(size = 14, color = colors),
            hoverinfo = "none",
            showlegend = FALSE
          )
      }

      # Se configura la vista y los ejes
      if (is.logical(labels) && labels) {
        fig <- fig %>%
          layout(
            title = list(
              text = paste("<b>Allocations for c = (", paste(c, collapse = ", "), ")</b>", sep = ""),
              font = list(
                family = "arial",  # Usa la fuente por defecto de R
                size = 19,         # Tamaño estándar similar al de R
                color = "black"    # Color negro como en los gráficos básicos
              ),
              y = 0.99  # Ajusta el valor de "y" para mover el título hacia abajo (0.95 es un valor más cerca del centro)
            ),
            scene = list(
              xaxis = list(title = x_title, range = c(0, NULL)),
              yaxis = list(title = y_title, range = c(0, NULL)),
              zaxis = list(title = z_title,range = c(0, 1.2*max(vertices[,3]))),
              camera = list(eye = list(x = 1.7, y = 1.7, z = 1.2))
            )
          )
      } else{
        fig <- fig %>%
          layout(scene = list(
            xaxis = list(title = x_title, range = c(0, NULL)),
            yaxis = list(title = y_title, range = c(0, NULL)),
            zaxis = list(title = z_title,range = c(0, 1.2*max(vertices[,3]))),
            camera = list(eye = list(x = 1.7, y = 1.7, z = 1.2))
          )
          )
      }
      print(fig)
    }

    # Real y 3D
    else if (dimension == "3D" && representation == "real") {

      # Caso 1: Los 3 primeros costes son nulos y el otro positivo
      if (c[1] == 0 && c[2] == 0 && c[3] == 0) {

        vertices <- matrix(c(
          0, 0, 0, c[4],
          c[1], 0, 0, c[4]-c[1],
          c[1], c[2]-c[1], 0, c[4]-c[2],
          0, c[2], 0, c[4]-c[2],
          0, 0, c[3], c[4]-c[3],
          c[1], 0, c[3]-c[1], c[4]-c[3],
          c[1], c[2]-c[1], c[3]-c[2], c[4]-c[3],
          0, c[2], c[3]-c[2], c[4]-c[3]
        ), nrow = 8, byrow = TRUE)

        # Se establecen las etiquetas de los vértices
        vertex_labels <- apply(vertices[1:8, ], 1, function(v) {
          sprintf("(%g, %g, %g, %g)", v[1], v[2], v[3], v[4])
        })


        # Se definen los puntos de la lista 'contributions'
        contributions_x <- sapply(contributions, function(contribution) contribution[1])
        contributions_y <- sapply(contributions, function(contribution) contribution[2])
        contributions_z <- sapply(contributions, function(contribution) contribution[3])

        # Se establecen las etiquetas de la lista 'contributions'
        if (is.null(contributions_names)) {
          contributions_labels <- sapply(contributions, function(c) {
            sprintf("(%g, %g, %g, %g)", c[1], c[2], c[3], c[4])
          })
        } else {
          contributions_labels <- contributions_names
        }

        # Construcción de la figura
        fig <- plot_ly()

        # Se establecen las etiquetas de los vértices
        if (is.logical(labels) && labels) {
          fig <- fig %>%
            add_trace(
              type = "scatter3d",
              mode = "text",
              x = 0,
              y = 0,
              z = 0,
              text = paste0("<b>", vertex_labels, "</b>"),
              textposition = "bottom center",
              textfont = list(size = 12, color = adjustcolor(col2, alpha.f = 0.8)),
              hoverinfo = "none",
              showlegend = FALSE
            )
        }

        # Se agregan puntos en los vértices
        fig <- fig %>%
          add_trace(
            type = "scatter3d", mode = "markers",
            x = 0, y = 0, z = 0,
            marker = list(size = 2.5, color = adjustcolor(col1, alpha.f = 0.8)),
            showlegend = FALSE
          )

        fig <- fig %>%
          add_trace(
            type = "scatter3d",
            mode = "markers",
            x = contributions_x,
            y = contributions_y,
            z = contributions_z,
            marker = list(size = 5, color = colors, symbol = "circle"),
            hoverinfo = "none",
            showlegend = FALSE
          )

        if (labels == TRUE) {
          fig <- fig %>%
            add_trace(
              type = "scatter3d",
              mode = "text",
              x = contributions_x,
              y = contributions_y,
              z = contributions_z ,
              text = paste0("<b>", contributions_labels, "</b>"),
              textfont = list(size = 14, color = colors),
              hoverinfo = "none",
              showlegend = FALSE
            )
        }

        # Se configura la vista y los ejes
        if (is.logical(labels) && labels) {
          fig <- fig %>%
            layout(
              title = list(
                text = paste("<b>Real NS set for c = (", paste(c, collapse = ", "), ")</b>", sep = ""),
                font = list(
                  family = "arial",  # Usa la fuente por defecto de R
                  size = 19,         # Tamaño estándar similar al de R
                  color = "black"    # Color negro como en los gráficos básicos
                ),
                y = 0.99  # Ajusta el valor de "y" para mover el título hacia abajo (0.95 es un valor más cerca del centro)
              ),
              scene = list(
                xaxis = list(title = "", range = c(0, NULL), showgrid = TRUE, showticklabels = FALSE),
                yaxis = list(title = "", range = c(0, NULL), showgrid = TRUE, showticklabels = FALSE),
                zaxis = list(title = "", range = c(0, NULL), showgrid = TRUE, showticklabels = FALSE),
                camera = list(eye = list(x = 1.3, y = 1.3, z = 1.3))
              )
            )

        } else{
          fig <- fig %>%
            layout(scene = list(
              xaxis = list(title = "", range = c(0, NULL), showgrid = TRUE, showticklabels = FALSE),
              yaxis = list(title = "", range = c(0, NULL), showgrid = TRUE, showticklabels = FALSE),
              zaxis = list(title = "", range = c(0, NULL), showgrid = TRUE, showticklabels = FALSE),
              camera = list(eye = list(x = 1.5, y = 1.5, z = 1.5))
            )
            )

        }

        print(fig)
      }

      # Resto de casos
      else {

        # Vértices del NS set en R4
        II <- matrix(c(
          -c[2]-c[3], c[2], c[3], c[4],
          c[1], -c[1]-c[3], c[3], c[4],
          c[1], c[2], -c[1]-c[2], c[4],
          c[1], c[2], c[3], c[4]-c[1]-c[2]-c[3]
        ), nrow = 4, byrow = TRUE)

        # Lado del tetraedro de imputaciones
        ll <- c[1] + c[2] + c[3]

        # Tetraedro en R3
        Tetraedro <- matrix(c(
          0, ll, ll/2, ll/2,
          0, 0, sqrt(3)/2 * ll, sqrt(3)/6 * ll,
          0, 0, 0, -sqrt(6)/3 * ll
        ), nrow = 3, byrow = TRUE)
        M <- Tetraedro %*% solve(t(II))
        # Rectángulo base en R4
        rectangulo <- matrix(c(
          0, 0, 0, c[4],
          c[1], 0, 0, c[4]-c[1],
          c[1], c[2], 0, c[4]-c[1]-c[2],
          0, c[2], 0, c[4]-c[2],
          0, 0, c[3], c[4]-c[3],
          c[1], 0, c[3], c[4]-c[1]-c[3],
          c[1], c[2], c[3], c[4]-c[1]-c[2]-c[3],
          0, c[2], c[3], c[4]-c[2]-c[3]
        ), nrow = 8, byrow = TRUE)

        # Proyección en el tetraedro en R3
        PuntosRectangulo <- M %*% t(rectangulo)
        PuntosRectangulo <- cbind(PuntosRectangulo, PuntosRectangulo[,1])

        # Extracción de las coordenadas
        x <- PuntosRectangulo[1, ]
        y <- PuntosRectangulo[2, ]
        z <- PuntosRectangulo[3, ]

        # Vértices del NS set:
        vertices <- matrix(c(
          0, 0, 0, c[4],
          c[1], 0, 0, c[4]-c[1],
          c[1], c[2]-c[1], 0, c[4]-c[2],
          0, c[2], 0, c[4]-c[2],
          0, 0, c[3], c[4]-c[3],
          c[1], 0, c[3]-c[1], c[4]-c[3],
          c[1], c[2]-c[1], c[3]-c[2], c[4]-c[3],
          0, c[2], c[3]-c[2], c[4]-c[3]
        ), nrow = 8, byrow = TRUE)

        # Proyección de los vértices en R3
        puntos <- M %*% t(vertices)

        # Se definen las caras del poliedro NS
        caras <- list(
          c(1,2,3,4),
          c(5,6,7,8),
          c(2,3,7,6),
          c(1,4,8,5),
          c(1,2,6,5),
          c(3,4,8,7)
        )

        # Se establecen las etiquetas de los vértices
        vertex_labels <- apply(vertices[1:8, ], 1, function(v) {
          sprintf("(%g, %g, %g, %g)", v[1], v[2], v[3], v[4])
        })

        # Se definen los puntos de la lista 'contributions'
        contributions_x <- sapply(contributions, function(contribution) contribution[1])
        contributions_y <- sapply(contributions, function(contribution) contribution[2])
        contributions_z <- sapply(contributions, function(contribution) contribution[3])

        # Se construye la matriz de 'contributions'
        contributions_matrix <- matrix(
          unlist(lapply(contributions, function(c) c[1:4])),
          ncol = 4, byrow = TRUE
        )

        # Proyección de las contribuciones en R3
        contribuciones <- M %*% t(contributions_matrix)

        # Se establecen las etiquetas de la lista 'contributions'
        if (is.null(contributions_names)) {
          contributions_labels <- sapply(contributions, function(c) {
            sprintf("(%g, %g, %g, %g)", c[1], c[2], c[3], c[4])
          })
        } else {
          contributions_labels <- contributions_names
        }

        # Construcción de la figura
        fig <- plot_ly()

        # Dibujo del poliedro NS
        for (cara in caras) {
          fig <- fig %>% add_trace(
            x = puntos[1, cara], y = puntos[2, cara], z = puntos[3, cara],
            type = "mesh3d",
            opacity = 0.3,
            facecolor = rep(col,12),
            showlegend = FALSE,
            hoverinfo = "none"
          )
        }

        # Se agregan las líneas del poliedro
        fig <- fig %>%
          add_trace(
            type = "scatter3d", mode = "lines",
            x = x[c(1,2,3,4,1)], y = y[c(1,2,3,4,1)], z = z[c(1,2,3,4,1)],
            line = list(color = adjustcolor(col, alpha.f = 0.8), width = 5), hoverinfo = "none", showlegend = FALSE
          ) %>%
          add_trace(
            type = "scatter3d", mode = "lines",
            x = x[c(5,6,7,8,5)], y = y[c(5,6,7,8,5)], z = z[c(5,6,7,8,5)],
            line = list(color = adjustcolor(col, alpha.f = 0.8), width = 5), hoverinfo = "none", showlegend = FALSE
          ) %>%
          add_trace(
            type = "scatter3d", mode = "lines",
            x = x[c(2,3,7,6,2)], y = y[c(2,3,7,6,2)], z = z[c(2,3,7,6,2)],
            line = list(color = adjustcolor(col, alpha.f = 0.8), width = 5), hoverinfo = "none", showlegend = FALSE
          ) %>%
          add_trace(
            type = "scatter3d", mode = "lines",
            x = x[c(1,4,8,5,1)], y = y[c(1,4,8,5,1)], z = z[c(1,4,8,5,1)],
            line = list(color = adjustcolor(col, alpha.f = 0.8), width = 5), hoverinfo = "none", showlegend = FALSE
          )

        # Se establecen las etiquetas de los vértices
        if (is.logical(labels) && labels) {
          fig <- fig %>%
            add_trace(
              type = "scatter3d",
              mode = "text",
              x = puntos[1, ],
              y = puntos[2, ],
              z = puntos[3, ],
              text = paste0("<b>", vertex_labels, "</b>"),
              textposition = "bottom center",
              textfont = list(size = 12, color = adjustcolor(col2, alpha.f = 0.8)),
              hoverinfo = "none",
              showlegend = FALSE
            )
        }

        # Se agregan puntos en los vértices
        fig <- fig %>%
          add_trace(
            type = "scatter3d", mode = "markers",
            x = puntos[1, ], y = puntos[2, ], z = puntos[3, ],
            marker = list(size = 2.5, color = adjustcolor(col1, alpha.f = 0.8)),
            showlegend = FALSE
          )

        fig <- fig %>%
          add_trace(
            type = "scatter3d",
            mode = "markers",
            x = contribuciones[1, ],
            y = contribuciones[2, ],
            z = contribuciones[3, ],
            marker = list(size = 5, color = colors, symbol = "circle"),
            hoverinfo = "none",
            showlegend = FALSE
          )

        if (labels == TRUE) {
          fig <- fig %>%
            add_trace(
              type = "scatter3d",
              mode = "text",
              x = contribuciones[1, ],
              y = contribuciones[2, ],
              z = contribuciones[3, ],
              text = paste0("<b>", contributions_labels, "</b>"),
              textfont = list(size = 14, color = colors),
              hoverinfo = "none",
              showlegend = FALSE
            )
        }

        # Se configura la vista y los ejes
        if (is.logical(labels) && labels) {
          fig <- fig %>%
            layout(
              title = list(
                text = paste("<b>Real NS set for c = (", paste(c, collapse = ", "), ")</b>", sep = ""),
                font = list(
                  family = "arial",  # Usa la fuente por defecto de R
                  size = 19,         # Tamaño estándar similar al de R
                  color = "black"    # Color negro como en los gráficos básicos
                ),
                y = 0.99  # Ajusta el valor de "y" para mover el título hacia abajo (0.95 es un valor más cerca del centro)
              ),
              scene = list(
                xaxis = list(title = "", range = c(0, NULL), showgrid = TRUE, showticklabels = FALSE),
                yaxis = list(title = "", range = c(0, NULL), showgrid = TRUE, showticklabels = FALSE),
                zaxis = list(title = "", range = c(0, NULL), showgrid = TRUE, showticklabels = FALSE),
                camera = list(eye = list(x = 1.5, y = 1.5, z = 1.5))
              )
            )
        } else{
          fig <- fig %>%
            layout(scene = list(
              xaxis = list(title = "", range = c(0, NULL), showgrid = TRUE, showticklabels = FALSE),
              yaxis = list(title = "", range = c(0, NULL), showgrid = TRUE, showticklabels = FALSE),
              zaxis = list(title = "", range = c(0, NULL), showgrid = TRUE, showticklabels = FALSE),
              camera = list(eye = list(x = 1.5, y = 1.5, z = 1.5))
            )
            )
        }

        print(fig)
      }
    }
  }

  if (!identical(original.c, c)){
    message("Warning: 'c' has been sorted in ascending order, and 'contributions' have been reordered accordingly to plot the graphic")
  }
}
