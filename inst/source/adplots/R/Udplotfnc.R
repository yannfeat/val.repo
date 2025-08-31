#' Creates Ud-plot for the provided data excluding and including the estimated normal density curve.
#'
#' @description Ud-plot developed by a slight modification of Ad-plot can be utilized to assess normality.
#' @usage udplot(X, npdf = FALSE, title = ifelse(npdf == FALSE, "Ud-plot",
#'        "Ud-plot & Normal Density Curve"), xlab = "x", lcol = "black",
#'        rcol = "grey60", pdfcol = "red", ...)
#' @param X an \eqn{n} by \eqn{1} matrix, equivalently, a column vector of length \eqn{n}, where \eqn{n} is the number of observations.
#' @param npdf display of the estimated normal density curve in the Ud-plot, \emph{FALSE} by default.
#' @param title title of the plot, \emph{Ud-plot} by default and \emph{Ud-plot & Normal Density Curve} otherwise.
#' @param xlab \eqn{x}-axis label, \eqn{x} by default.
#' @param lcol color of the points corresponding to the data that are less than or equal to the sample average, \emph{black} by default.
#' @param rcol color of the points corresponding to the data that are greater than the sample average, \emph{grey60} by default.
#' @param pdfcol color of the estimated normal density curve, \emph{red} by default.
#' @param ... other graphical parameters.
#'
#' @import ggplot2 stats
#' @return Ad-plot
#' @references Wijesuriya, U. A. (2025). Ad-plot and Ud-plot for Determining Distributional
#'             Characteristics and Normality.
#'             \emph{Communications in Statistics-Theory and Methods}, \doi{10.1080/03610926.2024.2440583}.
#' @examples
#'    set.seed(0)
#'    X1 <- matrix(rnorm(50, mean = 2, sd = 5))
#'    udplot(X1)
#'
#'    X2 <- matrix(rnorm(50, mean = 2, sd = 5))
#'    udplot(X2, npdf = TRUE)
#'
#'    X3 <- matrix(rnorm(500, mean = 2, sd = 5))
#'    udplot(X3, npdf = TRUE, title = "", lcol = "blue", rcol = "red", pdfcol = "black")
#' @export
udplot <- function(X, npdf = FALSE, title = ifelse(npdf == FALSE, "Ud-plot", "Ud-plot & Normal Density Curve"), xlab = "x", lcol = "black", rcol = "grey60", pdfcol = "red",...){
  D <- sort(X)                                                             # Sorts data in ascending order
  n <- length(D)                                                           # Computes sample size
  xbar <- mean(D)                                                          # Computes sample average
  s <- sd(D)                                                               # Computes sample standard deviation
  ylab <- parse(text=paste0("U[",n,"](x)/((1-",n,"^-1)*~",round(s,2),"^2)"))  # Provides y-axis label
  if (npdf == FALSE)
  {
    U <- matrix(NA, nrow = n, ncol = 1)                                    # Defines an NA column vector size n
  for (j in 1:n){
    U[j] <- round(-sum((D[which(D <= D[j])] - xbar))/n*1/((1 - n^{-1})*s^2), 10)      # Computes values of the Empirical Cumulative Average Deviation Function following Theorem 1
    }
df <- data.frame(D,U)                                                      # Creates a dataframe
g <- ggplot(df, aes(x = D))+
    geom_point(aes(x = D, y = U), col = ifelse(D <= xbar, lcol, rcol))+
    labs(title = title)+
    xlab(xlab)+
    ylab(ylab)+
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 12))  # Creates the ggplot
  }

  if (npdf==TRUE)
  {
  U <- matrix(NA, nrow = n, ncol = 1)                                      # Defines an NA column vector size n
  f <- matrix(NA, nrow = n, ncol = 1)                                      # Defines an NA column vector size n
  q <- matrix(NA, nrow = n, ncol = 1)                                      # Defines an NA column vector size n
  for (j in 1:n){
    U[j] <- round(-sum((D[which(D <= D[j])] - xbar))/n*1/((1 - n^{-1})*s^2), 10)      # Computes values of the Empirical Cumulative Average Deviation Function following Theorem 1
    f[j] <- round((1/sqrt(2*pi*s^2))*exp(-(1/(2*s^2))*(D[j] - xbar)^2), 10)           # Computes the normal density estimating the population mean and standard deviation by sample average and sample standard deviation, respectively
    q[j] <- round(min(U[j], f[j])/max(U[j], f[j]), 4)
  }
  d <- round(sum(q)/n, 2)                                                  # Computes the d-value
  df <- data.frame(D, U, f)                                                # Creates a dataframe
  g <- ggplot(df, aes(x = D))+
    geom_point(aes(x = D, y = U), col = ifelse(D <= xbar, lcol, rcol))+
    stat_function(fun = dnorm, args=list(mean = xbar,sd = s), colour = pdfcol)+
    labs(title = title)+
    xlab(xlab)+
    ylab(ylab)+
    annotate(geom = "label",x = Inf, y = Inf, parse = TRUE, col = "black", label = paste0('italic(d)-value ==', d), vjust=1.5, hjust = 1.1, size=4.5, fill = "grey92", label.size = NA)+
    theme(axis.text = element_text(size = 12),axis.title = element_text(size = 12))    # Creates the ggplot
  }
  return(g)
  }
