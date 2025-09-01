clonesCEBrule <- function(cw, eta, group_contribution = TRUE) { # Constrained equal benefits rule with clones
  
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
  
  original.order <- order(cw) # orden original de los costes
  cw <- sort(cw)    # cr es el vector de costes de los agentes sin costes clonados (en orden creciente)
  eta <- eta[original.order]  # # reordenación de eta en base a cómo lo ha sido cw
  cnull <- sum(cw==0) # Número de costes que son iguales 0
  n <- length(cw) # Número de agentes
  x <- numeric(n)  # Vector de contribuciones
  
  # Casuísticas:
  
  ## Caso 1: Todos los costes son iguales a 0
  if (cnull == n) {
    x <- numeric(n) # Todos los elementos del vector de contribuciones son 0
  }
  
  ## Caso 2: Al menos uno de los costes es estrictamente positivo (resto de casos)
  else{
    control <- FALSE  # Control para el bucle
    ii <- 1  # Índice inicial
    
    # Hay que encontrar el grupo de agentes con reparto no nulo
    while (!control) {
      cr <- pmax(cw - cw[ii], numeric(n)) # Vector cr como máximo entre la resta y el vector de ceros
      if (sum(eta * cr) < cw[n]) {
        control <- TRUE
      }
      ii <- ii + 1  # Se corrige el índice del primer agente con reparto no nulo
    }
    
    ii <- ii - 1 # El primer agente con reparto no nulo
    
    # Con los agentes ii hasta n, se calcula b tal que los beneficios c - x sean constantes e iguales a b
    b <- (sum(eta[ii:n] * cw[ii:n]) - cw[n]) / sum(eta[ii:n])
    
    # Se halla el reparto:
    x <- eta * pmax(cw - b, numeric(n)) # Reparto con máxima entre c - b y 0
  }
  
  # Se reordenan las contribuciones en base al orden original:
  x <- x[order(original.order)]
  
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
