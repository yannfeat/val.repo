wCEBrule <- function(c, w) { # Weighted constrained equal benefits rule

  # Verificación de que todos los costes sean no negativos (c ≥ 0)
  if (any(c < 0)) {
    stop("'c' must have nonnegative coordinates")
  }

  # Verificación de que todos los pesos sean estrictamente positivos (w > 0)
  if (any(w <= 0)) {
    stop("'w' must have positive coordinates")
  }

  # Verificación de que los vectores c y w tienen la misma longitud
  if (length(c) != length(w)) {
    stop("'c' and 'w' must have the same length")
  }

  original.order <- order(c) # orden original de los costes
  c <- sort(c)    # c es el vector de costes de los agentes (en orden creciente)
  w <- w[original.order] # reordenación de los pesos en base al orden de c
  w <- w / sum(w) # Normalización del vector de pesos
  n <- length(c)  # Número de agentes
  x <- numeric(n) # Vector de contribuciones
  b <- sum(c[1:(n - 1)]) # Beneficio a repartir
  Tol <- 1e-6 # Nivel de tolerancia

  # Conjunto de agentes por asignar: 1 = por asignar, 0 = asignado
  A <- rep(1, n)

  # Caso especial: n=1
  if (n == 1) {
    x <- c
  }

  # Resto de casos
  else {
    while (sum(A) > 0) {
      J <- which(A == 1)  # Agentes por asignar
      xtest <- x

      # Cálculo de las contribuciones propuestas para los agentes no asignados
      if (length(J) > 0) {
        xtest[J] <- c[J] - (w[J] / sum(w[J])) * b
      }

      # Verificación de las restricciones
      I0 <- xtest < 0
      I1 <- logical(n-1)

      # Comprobación de la restricción NS
      I1_part <- (cumsum(xtest[1:(n-1)]) > (c[1:(n-1)] + Tol))
      I1 <- I1_part & (A[1:(n-1)] == 1)

      # Caso 1: No hay contribuciones negativas ni se viola NS
      if (sum(I0) == 0 && sum(I1) == 0) {
        x <- xtest
        A[] <- 0
      }

      # Caso 2: No hay contribuciones negativas, pero se viola NS
      else if (sum(I0) == 0 && sum(I1) > 0) {
        k <- max(which(I1)) # Último agente que viola NS
        S <- which(A[1:k] == 1) # Agentes que quedan por asignar
        # Resolución del problema restringido
        xx <- wCEBrule(c[S], w[S])
        x[S] <- xx
        A[S] <- 0
        b <- b - sum(c[S] - x[S]) # Ajuste del beneficio restante
      }

      # Caso 3: Existencia de contribuciones negativas (beneficio excesivo)
      else if (sum(I0) > 0) {
        # Si el último agente presenta contribución negativa
        if (I0[n]) {
          x[n] <- c[n] - c[n-1] # Asignación de contribución máxima al última agente
          A[I0] <- 0 # Agentes con contribución negativa ya asignados
          sum_c_part <- sum(c[1:(n-1)][I0[1:(n-1)]]) # Ajuste del beneficio restante
          b <- b - sum_c_part - c[n-1]
        }
        else {
          A[I0] <- 0
          b <- b - sum(c[I0]) # Ajuste del beneficio restante
        }
      }
    }
  }

  # Se reordenan las contribuciones en base al orden original
  x <- x[original.order]

  # Se devuelve el vector de contribuciones
  return(x)
}
