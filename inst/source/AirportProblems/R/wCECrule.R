wCECrule <- function(c, w) { # Weighted constrained equal contributions rule
  
  # Verificación de que todos los costes sean no negativos (c=>0)
  if (any(c < 0)) {
    stop("'c' must have nonnegative coordinates")
  }
  
  # Verificación de que todos los pesos sean estrictamente positivos (w>0)
  if (any(w <= 0)) {
    stop("'w' must have positive coordinates")
  }
  
  # Verificación de que los vectores c y w tiene la misma longitud
  if (length(c) != length(w)) {
    stop("'c' and 'w' must have the same length")
  }
  
  original.order <- order(c) # orden original de los costes
  c <- sort(c)    # c es el vector de costes de los agentes (en orden creciente)
  w <- w[original.order] # reordenación de los pesos en base a cómo lo han sido los costes
  w <- w / sum(w) # Normalización del vector de pesos
  n <- length(c)  # Número de agentes
  x <- numeric(n) # Vector de contribuciones
  agente <- 0      # Índice del agente actual
  
  # Casuísticas:
  
  ## Caso 1: Si solo hay un único agente, su contribución es su coste asignado
  if (n == 1) {
    return(c)
  }
  
  ## Caso 2: Si hay dos agentes o más, se procede de la siguiente manera (resto de casos)
  else{
    
  # Función auxiliar WCEC1
  WCEC1 <- function(c, w) {
    w <- w / sum(w)  # Normalización del vector de pesos
    n <- length(c)   # Número de agentes
    R <- c[n] * w[1]  # Reparto inicial del último coste
    for (ii in (n-1):1) {
      R <- c(c[ii] * w[1] / sum(w[1:ii]), R)  # Reparto recursivo
    }
    x1 <- min(R)  # Mínimo reparto posible
    K <- max(which(R == x1))  # Índice del coste más alto asociado
    return(list(x1 = x1, K = K))
  }
  
  # Reparto del primer agente
  result <- WCEC1(c, w)
  x1 <- result$x1
  K <- result$K
  x[1:K] <- c[K] * w[1:K] / sum(w[1:K])
  agente <- agente + K
  # Iteración para los siguientes agentes
  while (agente < n) {
    agente <- agente + 1
    cr <- c[agente:n] - sum(x)
    pr <- w[agente:n]
    result <- WCEC1(cr, pr)
    x1 <- result$x1
    K <- result$K
    x[agente:(agente+K-1)] <- cr[K] * pr[1:K] / sum(pr[1:K])
    agente <- agente + K - 1
  }
  
  # Ajuste final para el último agente
  x[n] <- c[n] - sum(x[1:(n-1)])
  
  # Se eliminan los valores NA antes de devolver el resultado
  x <- x[!is.na(x)]
  }
  
  # Se reordenan las contribuciones en base al orden original
  x <- x[order(original.order)]
  
  # Se devuelve el vector de contribuciones
  return(x)
}

