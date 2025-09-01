clonesCPrule <- function(cw, eta, group_contribution = TRUE) { # Constrained proportional rule with clones

  # cw: Vector de costes sin agentes clonados (cada respectivo coste solo aparece una vez)
  # eta: Tamaño de cada grupo de agentes clonados
  # group_contribution: Si es TRUE, x nos devuelve la asignación agrupada de todos los agentes de cada grupo.
  # group_contribution: Si es FALSE, x nos devuelve la asignación que recibe solo uno de los agentes de cada grupo (esta asignación es la misma para todos, excepto para el caso de la PRIOR).

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

  # Llama a la función wSFCrule usando cw como c y eta como w
  x <- wCPrule(cw, eta)

  # Se define la salida en función del argumento 'group_contribution' establecido
  if (group_contribution == TRUE) {
    x <- x
  }
  else {
    x <- x / eta
  }

  # Se devuelve el vector de contribuciones:
  return(x)
}
