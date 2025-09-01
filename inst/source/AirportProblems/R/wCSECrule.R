wCSECrule <- function(c, w) { # Weighted coalitions sequential equal contributions rule
  
  # Verificación de que todos los costes sean no negativos (c=>0)
  if (any(c < 0)) {
    stop("'c' must have nonnegative coordinates")
  }
  
  # Verificación de que todos los pesos sean no negativos (w=>0)
  if (any(w < 0)) {
    stop("'w' must have nonnegative coordinates")
  }
  
  # Verificación de que no todos los pesos son negativos (Σwi!=0)
  if (all(w == 0)) {
    stop("'w' must have at least one positive coordinate")
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
  W <- cumsum(w) # Cálculo acumulativo de los pesos
  
  # Casuísticas:
  
  ## Caso 1: Si solo hay un único agente, su contribución es su coste asignado
  if (n == 1) {
    return(c)
  }
  
  ## Caso 2: Si hay dos agentes o más, se procede de la siguiente manera (resto de casos)
  
  # Cálculo de las contribuciones para cada agente i
  for (i in 1:n) {
    # Término directo
    x[i] <- c[i] * w[i]
    
    # Términos acumulativos
    if (i > 1) {
      for (j in 2:i) {
        x[i] <- x[i] + (c[j] - c[j - 1]) * (W[j - 1] / (n - j + 1))
      }
    }
  }
  
  # Se reordenan las contribuciones en base al orden original
  x <- x[order(original.order)]
  
  # Se devuelve el vector de contribuciones
  return(x) 
}
