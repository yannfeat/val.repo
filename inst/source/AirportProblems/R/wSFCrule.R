wSFCrule <- function(c, w) { # Weighted sequential full contributions rule
  
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
 
  # Se identifican los grupos de agentes que comparten el mismo coste
  unique_costs <- unique(c)  # Costes únicos
  groups <- split(seq_along(c), c)  # Se agrupan los índices por coste
  
  # Inicialización de la variable coste:
  prev_cost <- 0
  
  # Calcular las contribuciones grupo por grupo
  for (cost in unique_costs) {
    group_indices <- groups[[as.character(cost)]] # Índices de los agentes en un grupo
    group_weights <- w[group_indices] # Pesos de los agentes en el grupo
    incremental_cost <- cost - prev_cost # Coste incremental del grupo
    total_group_weight <- sum(group_weights) # Suma de pesos del grupo
    x[group_indices] <- incremental_cost * (group_weights / total_group_weight) # Distribución del coste incremental proporcionalmente según los pesos
    prev_cost <- cost # Actualización del coste previo
  }
  
  # Se reordenan las contribuciones en base al orden original
  x <- x[order(original.order)]
  
  return(x) # Se devuelve el vector de contribuciones
}
  