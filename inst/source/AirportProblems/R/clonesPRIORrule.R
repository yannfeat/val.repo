clonesPRIORrule <- function(cw, eta, group_contribution = TRUE, order = NULL) { # Priority rule relative with clones

  # cw: Vector de costes sin agentes clonados (cada respectivo coste solo aparece una vez)
  # eta: Tamaño de cada grupo de agentes clonados
  # Si no se proporciona ningún orden específico, definimos que por defecto se tome un orden creciente (1,2,...,n)
  # group_contribution: Si es TRUE, x nos devuelve la asignación agrupada de todos los agentes de cada grupo.
  # group_contribution: Si es FALSE, x nos devuelve la asignación que recibe solo uno de los agentes de cada grupo (esta asignación es la misma para todos, excepto para el caso de la PRIOR).
  if (is.null(order)) {
    order <- 1:length(cw)
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

  ## Requerimiento 4: El vector "order" debe contener cada índice desde 1 hasta n exactamente una vez
  if (length(order) != length(cw) || any(sort(order) != 1:length(cw))) {
    stop("The 'order' vector must contain each index from 1 to n exactly once")
  }

  ## Aviso: El vector de costes cw no ha de incluir clones
  if (length(cw) != length(unique(cw))) {
    warning("'cw' has cloned agents")
  }

  # Llama a la función PRIORrule usando cw como c y eta como w
  return(PRIORrule(cw, order))
}
