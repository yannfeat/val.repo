rmultreg <- function(n, k = 1, minimum = 0, maximum = 1, p = 0.5, dfnoise = 100, sdnoise = 1) { 
    beta <- runif(n = k+1, min = minimum, max = maximum)*rbinom(n = k+1, size = 1, prob = p)
    xy <- data.frame(matrix(rnorm(k*n), nrow = n))
    names(xy) <- paste("x", 1 : k, sep = "")
    noise <-  rt(n, df = dfnoise)*sdnoise*sqrt((dfnoise - 2)/dfnoise)
    y <- as.matrix(xy)%*%beta[-1] + beta[1] + noise
    names(beta) <- c("Intercept", names(xy))
    xy$y <- as.vector(y) 
    names(xy)[k+1] <- "y"
    return(list(data = xy, coefficients = beta))
}
