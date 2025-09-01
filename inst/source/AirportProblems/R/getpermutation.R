getpermutation <- function(n, num) { # Principio de la función

  ################################
  ### Comprobación datos entrada###
  ################################

  if (num < 0 || num %% 1 != 0) {
    stop("In getpermutation(n,num), num must be a positive integer", call. = F)
  }

  if (n < 0 || n %% 1 != 0) {
    stop("In getpermutation(n,num), n must be a positive integer", call. = F)
  }

  if (num > factorial(n)) {
    stop("In getpermutation(n,num), num must be smaller or equal than n!", call. = F)
  }

  ################################
  ###### Cuerpo de la función######
  ################################

  # Primero calculamos la codificación de Lehmer de la permutación número num.  El código de Lehmer
  # cuenta el número de términos en la permutación a la derecha de cada uno que son más pequeños que él.
  num <- num - 1
  d <- rep(0, n)
  divisor <- 2 # Hacemos todas las divisiones empezando por 2 hasta encontrar un 0.
  while (num != 0) {
    d[n - divisor + 1] <- num %% divisor
    num <- trunc(num / divisor)
    divisor <- divisor + 1
  }

  # Hacemos la descodificación como se indica en la página de wikipedia:
  # For decoding a Lehmer code into a permutation of a given set: for each entry x,
  # in order from right to left, correct the items to its right by adding 1 to all
  # those (currently) greater than or equal to x; finally interpret the resulting
  # permutation of {0, 1, … n − 1} as sequence numbers (which amounts to adding 1
  # to each entry if a permutation of {1, 2, … n} is sought).

  permu <- d

  for (ii in (n - 1):1)
  {
    permu[(ii + 1):length(permu)] <- permu[(ii + 1):length(permu)] + as.numeric(permu[ii] <= permu[(ii + 1):length(permu)])
  }

  permu <- permu + 1 # La ordenación parte del 0, por lo que añadimos un 1.
  ################################
  ###### Salidas de la función#####
  ################################

  return(permu)
} # Fin de la función
