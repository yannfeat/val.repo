volume <- function(c) {
  # Calcula el volumen V(c1, c2, ..., cn)
  # c debe ser un vector numérico de costes

  c <- sort(c)

  n <- length(c)
  matriz <- matrix(0, nrow = n, ncol = n)

  for (k in 1:n) {
    matriz[k, k] <- prod(rep(c[k], k) / (1:k))
    if (k > 1) {
      for (j in 1:(k - 1)) {
        matriz[k, j] <- -prod(rep(c[k] - c[j], k - j + 1) / (1:(k - j + 1)))
      }
    }
  }

  if (n > 2) {
    for (k in 2:(n - 1)) {
      for (r in (k + 1):n) {
        matriz[r, k] <- sum(matriz[r, k] * matriz[k - 1, 1:(k - 1)])
      }
    }
  }

  V <- sum(matriz[n, ])
  return(V)
}
