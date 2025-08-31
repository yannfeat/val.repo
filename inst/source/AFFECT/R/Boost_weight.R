W <- function(r_star,g_x){
  a <- data.frame(r_star); b <- data.frame(g_x)
  data <- cbind(a,b)
  colnames(data) <- c('Y',"X")
  w <- lm(Y~X,data = data)
  return(w$coefficients)
}
