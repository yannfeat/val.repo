wCPrule <- function(c, w) {# Weighted constrained proportional rule
  
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
  n <- length(c)  # Número de agentes
  x <- numeric(n) # Vector para almacenar las contribuciones
  
  ## Caso 1: Si todos los costes son cero, o hay solo un agente
  if (n == 1 || all(c == 0)) {
    return(c)
  }
  
  ## Caso 2: Para el resto de casos se procede de la siguiente manera
  else {
    # Se asigna una contribución de x_i=0 a todos aquellos costes nulos:
    non_zero_indices <- which(c != 0)
    zero_indices <- which(c == 0)
    
    c_non_zero <- c[non_zero_indices]
    w_non_zero <- w[non_zero_indices]
    
    # Subfunción WCP1 para los costes distintos de 0
    WCP1 <- function(c_non_zero, w_non_zero) {
      n <- length(c_non_zero)  # Número de agentes en el grupo actual
      p <- w_non_zero * c_non_zero  # Vector combinado de pesos (ponderación por costes)
      r <- numeric(n) # Vector de contribuciones proporcionales temporales
      
      # Cálculo de las contribuciones proporcionales
      for (ii in 1:n) {
        r[ii] <- p[1] / sum(p[1:ii]) * c_non_zero[ii]
      }
      
      k <- min(r, na.rm = TRUE) # La menor contribución proporcional
      J <- max(which(r == k))   # Índice máximo que satisface la regla de no-subsidio
      
      # Asignación proporcional hasta el agente J
      xJ <- numeric(J)     
      xJ[1:J] <- p[1:J] / sum(p[1:J]) * c_non_zero[J]
      
      return(list(xJ = xJ, J = J))
    }
    
    # Cálculo de las contribuciones para los agentes con costes no nulos
    result <- WCP1(c_non_zero, w_non_zero)  # Primer grupo de agentes
    x_non_zero <- numeric(length(c_non_zero))
    x_non_zero[1:result$J] <- result$xJ
    agente <- result$J  # Se actualiza el índice del último agente
    
    while (agente < length(c_non_zero)) {
      cr <- c_non_zero[(agente + 1):length(c_non_zero)] - sum(x_non_zero)
      pr <- w_non_zero[(agente + 1):length(c_non_zero)]
      result <- WCP1(cr, pr)
      xJ <- result$xJ
      J <- result$J
      x_non_zero[(agente + 1):(agente + J)] <- xJ
      agente <- agente + J
    }
    
    # Asignamos las contribuciones calculadas a los índices correspondientes
    x[non_zero_indices] <- x_non_zero
    x[zero_indices] <- 0
  }
  
  # Se reordenan las contribuciones en base al orden original
  x <- x[order(original.order)]
  
  return(x) # Se devuelve el vector de contribuciones
}

