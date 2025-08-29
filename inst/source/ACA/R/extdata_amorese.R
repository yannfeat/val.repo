#' Dataset amorese.data.txt
#'
#' This data set is a small simulated time series to test the \code{ACA} 
#' package.
#'
#' @name amorese.data.txt
#' 
#' @format This data set contains 2 columns. The first column is an index, 
#' from 1 to 410. The second column are the values of a synthetic 
#' combination of normal distributions. This is a modified version of the 
#' data example from James & Mattesons (2014) study: a sequence of 100 
#' independent samples from normal distributions (N(0, 1), N(0, 3), N(2, 1) 
#' and N(2, 4)). The notation N(??, ??) means normally distributed 
#' with mean ?? and standard deviation ??. This synthetic data set is 
#' slighty upgraded by adding an extra N(0, 3) very short (10 samples) 
#' segment at the end of the initial sequence. This extra tip is added in 
#' order to assess the detection capability for a breakpoint close to 
#' series??? end, where an edge effect may be significant. Moreover, a 5 per 
#' cent slope is added to this synthetic series to simulate a series with 
#' upward trend. This synthetic series is plotted in Figures 2b and 2d in 
#' Amorese & al. (2018).                        
#'
#' @source James, N.A. & Matteson, D.S., ecp: an R package for 
#' nonparametric multiple change point analysis of multivariate 
#' data, \emph{J. Stat. Softw.}, 62(7), 1???25 (2014).
#'
#' Amorese, D., Grasso, J. R., Garambois, S., and Font, M., 
#' "Change-point analysis of geophysical time-series: application 
#' to landslide displacement rate (Sechilienne rock avalanche, 
#' France)", \emph{Geophysical Journal International}, 213(2), 
#' 1231-1243 (2018).
#'
NULL