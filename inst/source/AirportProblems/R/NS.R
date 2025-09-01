NS <- function(x, c, tol = 1e-6, eta = rep(1, length(x)), group_contribution = TRUE, coalition = FALSE) {

  if (group_contribution == FALSE) {
    x <- x * eta  # Se obtiene la contribución agrupada de cada grupo
  }

  n <- length(c)  # Número de agentes

  # Comprobaciones:

  ## Requerimiento 1: Todas las coordenadas de los vectores x y c han de ser no negativas
  if (any(x < 0) && any(c < 0)) {
    stop("'x' and 'c' must have nonnegative coordinates")
  }
  if (any(x < 0)) {
    stop("'x' must have nonnegative coordinates")
  }
  if (any(c < 0)) {
    stop("'c' must have nonnegative coordinates")
  }

  ## Requerimiento 2: Los vectores x y c han de tener la misma longitud
  if (length(x) != n) {
    stop("'x' and 'c' must have the same length")
  }

  ## Requerimiento 3: La suma de todas las coordenadas de x debe ser aproximadamente igual a la coordenada más alta de c
  sum_x <- sum(x)
  max_c <- max(c)

  if (abs(sum_x - max_c) > tol) {
    stop("The sum of the coordinates of each element of 'contributions' must equal the maximum of 'c'.")
  }

  # Caso especial: Solo hay un único agente
  if (n == 1) {
    return(abs(x - c) < tol)  # Solo se verifica si x es igual a c
  }

  original.order <- order(c)  # Orden original de los costes
  c <- c[original.order]  # Vector de costes de los agentes (en orden creciente)
  x <- x[original.order]  # Vector de asignaciones de los agentes (en base al orden establecido para c)

  flag <- TRUE  # Si se cumplen todas las restricciones NS, flag=TRUE. En caso contrario, flag=FALSE.
  failing.coalition <- NULL  # Primera coalición donde se produce la violación de la restricción NS (si es que se produce)

  # Verificación de las condiciones de no-subsidio con tolerancia
  for (i in 1:(n-1)) {
    if ((sum(x[1:i]) - c[i]) > tol) {
      flag <- FALSE
      failing.coalition <- sort(original.order[1:i])
      break
    }
  }

  # Devolución de la salida según el valor que tome coalition (TRUE o FALSE).
  if (coalition == TRUE) {
    return(list(flag = flag, failing.coalition = failing.coalition))
  } else {
    return(flag)
  }
}
