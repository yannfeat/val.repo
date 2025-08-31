#' Creates Ad-plot for the provided data.
#'
#' @description Ad-plot identifies the characteristics of the distribution such as symmetry, skewness, and outliers of the data set.
#' @usage adplot(X, title = "Ad-plot", xlab = "x", lcol = "black", rcol = "grey60", ...)
#' @param X an \eqn{n} by \eqn{1} matrix, equivalently, a column vector of length \eqn{n}, where \eqn{n} is the number of observations.
#' @param title title of the plot, \emph{Ad-plot} by default.
#' @param xlab \eqn{x}-axis label, \eqn{x} by default.
#' @param lcol color of the points corresponding to the data that are less than or equal to the sample average, \emph{black} by default.
#' @param rcol color of the points corresponding to the data that are greater than the sample average, \emph{grey60} by default.
#' @param ... other graphical parameters.
#'
#' @import ggplot2
#' @return Ad-plot
#' @references Wijesuriya, U. A. (2025). Ad-plot and Ud-plot for Determining Distributional
#'             Characteristics and Normality.
#'             \emph{Communications in Statistics-Theory and Methods}, \doi{10.1080/03610926.2024.2440583}.
#' @examples
#'    set.seed(0)
#'    X1 <- matrix(rnorm(50, mean = 2, sd = 5))
#'    adplot(X1)
#'
#'    X2 <- matrix(rf(50, df1 = 10, df2 = 5))
#'    adplot(X2)
#'
#'    X3 <- matrix(rbeta(50, shape1 = 10, shape2 = 2))
#'    adplot(X3, title="", lcol = "blue", rcol = "red")
#' @export
adplot <- function(X, title = "Ad-plot", xlab = "x", lcol = "black", rcol = "grey60",...){
  D <- sort(X)                                     # Sorts data in ascending order
  n <- length(D)                                   # Computes sample size
  xbar <- mean(D)                                  # Computes sample average
  U <- matrix(NA, nrow = n, ncol = 1)              # Defines an NA column vector size n
  ylab <- parse(text = paste0('U[',n,'](x)'))      # Provides y-axis label
  for (j in 1:n){
    U[j] <- -sum((D[which(D <= D[j])] - xbar))/n   # Computes values of the Empirical Cumulative Average Deviation Function
  }
  df <- data.frame(D, U)                           # Creates a dataframe
  g <- ggplot(df, aes(x = D))+
    geom_point(aes(x = D, y = U), col = ifelse(D <= xbar, lcol, rcol))+
    labs(title = title)+
    xlab(xlab)+
    ylab(ylab)+
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 12))   # Creates the ggplot
  return(g)
}
