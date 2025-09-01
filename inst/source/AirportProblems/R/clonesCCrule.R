clonesCCrule <- function(cw, eta, group_contribution = TRUE) { # Core-center rule with clones
  # cw: Vector de costes sin agentes clonados (cada respectivo coste solo aparece una vez)
  # eta: Tamaño de cada grupo de agentes clonados
  # group_contribution: Si es TRUE, x nos devuelve la asignación agrupada de todos los agentes de cada grupo.
  # group_contribution: Si es FALSE, x nos devuelve la asignación que recibe solo uno de los agentes de cada grupo (esta asignación es la misma para todos, excepto para el caso de la PRIOR).

  # Verificación de que 'cw' es un vector numérico
  if (!is.numeric(cw)){
    stop("'cw' must be a numeric vector")
  }

  # Verificación de que todos los costes sean no negativos (c=>0)
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

  c <- rep(cw, eta) # Vector de costes en forma extendida
  original.order <- order(c) # orden original de los costes
  c <- sort(c)    # c es el vector de costes de los agentes (en orden creciente)
  cnull <- sum(c==0) # Número de costes que son iguales 0
  n <- length(c)   # Número de agentes
  x <- numeric(n)  # Vector de contribuciones

  # Warning para n>150:
  if (n > 150) {
    message("Warning: The execution time may be significantly longer")
  }

  # Casuísticas:

  ## Caso 1: Todos los costes son iguales a 0
  if (cnull == n) {
    x <- numeric(n) # Todos los elementos del vector de contribuciones son 0
  }

  ## Caso 2: Únicamente uno de los costes es positivo
  else if (cnull == n-1) {
    x <- numeric(n)
    x[n] <- c[n]
  }

  ## Caso 3: Dos costes son estrictamente positivos
  else if (cnull == n-2) {
    x <- numeric(n)
    x[n-1] <- c[n-1] / 2
    x[n] <- c[n] - c[n-1] / 2
  }

  ## Caso 4: Todos los costes son iguales
  else if (length(unique(c)) == 1) {
    x <- c / n
  }

  ## Caso 5: Todos los costes son estrictamente positivos
  else if (cnull == 0) {
    # Verificación de que volume() no reporta un valor nulo ni NA
    if (volume(c) == 0 || is.na(volume(c))){
      stop("The sum of the coordinates of 'c' is too high to apply this rule.")
    } else {
    V <- volume(c[1:(n-1)])  # Volumen para los primeros n-1 agentes
    for (k in 1:(n - 1)) {
      if ((k + 1) <= (n - 1)) {
        # Si el rango es válido
        A <- c[(k + 1):(n - 1)]
      } else {
        # Si el rango no es válido, generamos un vector vacío
        A <- numeric(0)
      }
      x[k] <- volume(c(c[1:k], c[k], A)) / V
    }
    x[n] <- c[n] - sum(x[1:(n - 1)])
    }
  }

  ## Caso 5: Algunos costes son nulos, pero no todos (al menos 3 no lo son)
  else {
    # Verificación de que volume() no reporta un valor nulo ni NA
    if (volume(c) == 0 || is.na(volume(c))) {
      stop("The sum of the coordinates of 'c' is too high to apply this rule.")
    } else{
    pos <- c[(cnull + 1):n]  # Costes estrictamente positivos (c > 0)
    xnull <- rep(0, cnull)   # Vector de contribuciones para los agentes con coste nulo
    xpos <- numeric(n - cnull)  # Se inicializa el vector de contribuciones para los agentes con coste positivo

    V <- volume(pos[1:(length(pos) - 1)])  # Volumen inicial sin el último elemento
    for (k in 1:(length(pos) - 1)) { # Se calculan todas las coordenadas excepto la última
      if (k == length(pos) - 1) {
        # Caso especial cuando k es el penúltimo elemento
        xpos[k] <- volume(c(pos[1:k], pos[k])) / V
      } else {
        xpos[k] <- volume(c(pos[1:k], pos[k], pos[(k + 1):(length(pos) - 1)])) / V
      }
    }

    xpos[length(pos)] <- pos[length(pos)] - sum(xpos[1:(length(pos) - 1)])  # Contribución del último agente

    x <- c(xnull, xpos) # Se combinan las contribuciones de los agentes con coste nulo y los de coste positivo
    }
  }

  # Se reordenan las contribuciones en base al orden original
  x <- x[order(original.order)]

  # Se reagrupan las contribuciones por los valores únicos de 'cw'
  if (group_contribution == TRUE) {
  x_agrupado <- as.vector(tapply(x, rep(seq_along(cw), eta), sum))
  }
  else {
  x_agrupado <- x[cumsum(eta)]
  }

  # Se devuelve el vector de contribuciones
  return(x_agrupado)
}
