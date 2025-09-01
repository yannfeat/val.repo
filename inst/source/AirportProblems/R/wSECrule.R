wSECrule <- function(c, w) { # Weighted sequential equal contributions rule
  
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
  
  # Casuísticas:
  
  ## Caso 1: Si solo hay un único agente, su contribución es su coste asignado
  if (n == 1) {
    return(c)
  }
  
  ## Caso 2: Si hay dos agentes o más, se procede de la siguiente manera (resto de casos)
  else{
  x <- w * c[1] # Contribución del primer agente
  
  # Los demás valores se calculan por recurrencia
  for (ii in 2:n) {
    x[ii:n] <- x[ii:n] + 
      (w[ii:n] / sum(w[ii:n])) * (c[ii] - c[ii-1])
  }
  }
  
  # Se reordenan las contribuciones en base al orden original
  x <- x[order(original.order)]
  
  # Se devuelve el vector de contribuciones
  return(x) 
}
