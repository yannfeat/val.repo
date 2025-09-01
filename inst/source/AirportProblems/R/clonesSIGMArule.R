clonesSIGMArule <- function(cw, eta, a = NULL) { # Slack maximizer rule with clones
  
  # cw: Vector de costes sin agentes clonados (cada respectivo coste solo aparece una vez)
  # eta: Tamaño de cada grupo de agentes clonados
  # a: Parámetro a (0 <= a <= 1). Si a=0, es equivalente a aplicar la CEC rule. Si a=1, es equivalente a aplicar la SM rule.
  # Si no se proporciona ningún parámetro a específico, definimos que por defecto se tome a = 0.5
  if (is.null(a)) {
    a <- 0.5
  }
  
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
  
  ## Requerimiento 4: El parámetro a ha de ser un único número y estar en el rango [0, 1]
  if (!is.numeric(a) || length(a) != 1) {
    stop("The parameter 'a' must be a single numeric value in the range [0,1]")
  }
  if (a < 0 || a > 1) {
    stop("The parameter 'a' must be a single numeric value in the range [0,1]")
  }
  
  ## Aviso: El vector de costes cw no ha de incluir clones
  if (length(cw) != length(unique(cw))) {
    warning("'cw' has cloned agents")
  }
  
  original.order <- order(cw) # orden original de los costes
  cw <- sort(cw)    # cw es el vector de costes de los agentes (en orden creciente)
  eta <- eta[original.order]  # reordenación de eta en base a cómo lo ha sido cw
  M <- cumsum(eta) # Suma acumulada de los tamaños de los grupos
  n <- length(cw)  # Número de agentes
  x <- numeric(n) # Vector de contribuciones
  
  # Casuísticas:
  
  ## Caso 1: Si solo hay un único agente, su contribución es su coste asignado
  if (n == 1) {
    return(cw)
  }
  
  ## Caso 2: Si hay dos agentes o más, se procede de la siguiente manera (resto de casos)
  else{
    
    # Contribución del primer agente
    x[1] <- eta[1] * min(c(cw[1:(n - 1)] / (M[1:(n - 1)] + a), cw[n] / M[n]))
    
    # Contribución de cada agente
    for (ii in 2:(n - 1)) {
      x[ii] <- eta[ii] * min(c((cw[ii:(n - 1)] - sum(x)) / (M[ii:(n - 1)] - M[ii - 1] + a), (cw[n] - sum(x)) / (M[n] - M[ii - 1])))
    }
    
    # Contribución del último agente
    x[n] <- cw[n] - sum(x[1:(n - 1)])
  }
  
  # Se reordenan las contribuciones en base al orden original:
  x <- x[order(original.order)]
  
  return(x)  # Se devuelve el vector de contribuciones
}
