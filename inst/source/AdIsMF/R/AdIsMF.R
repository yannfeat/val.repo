
#' @title Freundlich Linear Model

#' @description This model will fit the adsorption data to the linear form of the Freundlich equation and will give the estimates of the Freundlich parameters, namely "kf" and "1/n" while evaluating the performance efficiency of the linear model of Freundlich through several error functions.

#' @param ce Equilibrium concentration of the adsorbate in the solution

#' @param qe Amount adsorbed
#' @import AICcmodavg ggplot2 stats
#' @return
#' \itemize{
#'   \item Freundlich Isotherm Linear Model: Model summary
#'   \item correlation (ce, qe): Correlation between ce and qe
#'   \item kf: Freundlich constant
#'   \item 1/n: Freundlich exponent related to adsorption intensity
#'   \item AIC: Akaike information criterion
#'   \item AICc: Corrected Akaike information criterion
#'   \item BIC: Bayesian information criterion
#'   \item RMSE: Root Mean Squared Error
#'   \item MSE: Mean Squared Error
#'   \item MAE: Mean Absolute Error
#'   \item MAPE: Mean Absolute Percentage Error
#'   \item Chi.square: Chi-square value
#' }


#' @export
#' @usage FLM (ce, qe)

#' @examples
#' ce <- c(0.025, 0.04, 0.055, 0.099, 0.139, 0.402, 1.999, 11.336)
#' qe <- c(17.21, 35.42, 51.238, 72.659, 89.268, 182.21, 345.29, 634.231)
#' m.fit <- FLM (ce, qe)


#' @references
#' \itemize{

#' \item Giles, C. H. (1973). The history and use of the Freundlich adsorption isotherm. Journal of the Society of Dyers and Colourists, 89(8), 287-291.

#' \item Datta, S. P., Bhadoria, P. B. S., & Kar, S. (1998). Availability of extractable boron in some acid soils, West Bengal, India. Communications in soil science and plant analysis, 29(15-16), 2285-2306.

#' \item Roy, A., Manjaiah, K. M., Datta, S. P., Rakshit, D., Barman, M., Ray, P., Golui, D., Raza, M. B., Tigga, P., Mondal, S., Vishwanath, Meena, S., & Meena, P. (2025). Effect of Low-Molecular-Weight Organic Acids and Silicon on Arsenic Adsorption and Desorption in a Paddy Soil of Bengal Delta Plain: Insights from Thermodynamics and Equilibrium Modeling. Water, Air, & Soil Pollution, 236(6), 344.
#' \item Raza, M. B., Datta, S. P., Golui, D., Barman, M., Ray, P., Upadhyay, D., Mishra, R., Roy, A., & Dash, A. K. (2025). Enhancing soil arsenic immobilization with organic and inorganic amendments: Insights from sorption–desorption study. Environmental Monitoring and Assessment, 197(1), 76.
#' }


FLM<-function (ce, qe) {
  x <- log10(ce)
  y <- log10(qe)
  data <- data.frame(x, y)

  fit <- lm(y ~ x)

  fitted <- fit$fitted.values
  qe.fitted <- 10^fitted


  c <- summary(fit)
  a <- c$coefficients[1]
  b <- c$coefficients[2]
  kf <- 10^(a)

  n <- b


  aic <- AIC(fit)
  aicc <- AICcmodavg::AICc(fit)
  bic <- BIC(fit)


  rmse <-sqrt(mean((qe - qe.fitted)^2))
  mse <- mean((qe - qe.fitted)^2)
  mae <- mean(abs(qe - qe.fitted))
  mape <- (mean(abs(qe - qe.fitted)/qe))*100
  chi.sq <- sum(((qe - qe.fitted)^2)/qe.fitted)
  correl <- cor.test(ce, qe)


  adsorption_plot <- ggplot(data, aes(x = x, y = y)) +
    geom_point(color = "blue") +
    geom_smooth(formula = y ~ x, method = "lm", se = FALSE, color = "red") +
    labs(x = "log(ce)", y = "log(qe)", title = "Freundlich Isotherm Linear Model") +
    theme(plot.title = element_text(hjust = 0.5))

  output<- list("Freundlich Isotherm Linear Model"= summary(fit),
                'correlation (ce, qe)' = correl,
                kf = kf,
                '1/n' = b,
                AIC = aic,
                AICc = aicc,
                BIC = bic,
                RMSE = rmse,
                MSE = mse,
                MAE = mae,
                'MAPE (%)' = mape,
                Chi.square = chi.sq,
                Plot = adsorption_plot)

  return(output)

}















#' @title Freundlich Nonlinear Model

#' @description This model will fit the adsorption data to the nonlinear form of the Freundlich equation and will give the estimates of the Freundlich parameters, namely "kf" and "1/n" while evaluating the performance efficiency of the nonlinear model of Freundlich through several error functions.

#' @param ce Equilibrium concentration of the adsorbate in the solution

#' @param qe Amount adsorbed

#' @import AICcmodavg ggplot2 nls2 stats

#' @return
#' \itemize{
#'   \item Freundlich Isotherm Nonlinear Model: Model summary
#'   \item correlation (ce, qe): Correlation between ce and qe
#'   \item kf: Freundlich constant
#'   \item 1/n: Freundlich exponent related to adsorption intensity
#'   \item AIC: Akaike information criterion
#'   \item AICc: Corrected Akaike information criterion
#'   \item BIC: Bayesian information criterion
#'   \item RMSE: Root Mean Squared Error
#'   \item MSE: Mean Squared Error
#'   \item MAE: Mean Absolute Error
#'   \item MAPE: Mean Absolute Percentage Error
#'   \item MPSD: Marquardt’s Percent Standard Deviation
#'   \item Chi.square: Chi-square value
#' }

#' @export
#' @usage FNLM (ce, qe)

#' @examples
#' ce <- c(0.025, 0.04, 0.055, 0.099, 0.139, 0.402, 1.999, 11.336)
#' qe <- c(17.21, 35.42, 51.238, 72.659, 89.268, 182.21, 345.29, 634.231)
#' m.fit <- FNLM (ce, qe)


#' @references
#' \itemize{

#' \item Giles, C. H. (1973). The history and use of the Freundlich adsorption isotherm. Journal of the Society of Dyers and Colourists, 89(8), 287-291.

#' \item Datta, S. P., Bhadoria, P. B. S., & Kar, S. (1998). Availability of extractable boron in some acid soils, West Bengal, India. Communications in soil science and plant analysis, 29(15-16), 2285-2306.
#' \item Roy, A., Manjaiah, K. M., Datta, S. P., Rakshit, D., Barman, M., Ray, P., Golui, D., Raza, M. B., Tigga, P., Mondal, S., Vishwanath, Meena, S., & Meena, P. (2025). Effect of Low-Molecular-Weight Organic Acids and Silicon on Arsenic Adsorption and Desorption in a Paddy Soil of Bengal Delta Plain: Insights from Thermodynamics and Equilibrium Modeling. Water, Air, & Soil Pollution, 236(6), 344.
#' \item Raza, M. B., Datta, S. P., Golui, D., Barman, M., Ray, P., Upadhyay, D., Mishra, R., Roy, A., & Dash, A. K. (2025). Enhancing soil arsenic immobilization with organic and inorganic amendments: Insights from sorption–desorption study. Environmental Monitoring and Assessment, 197(1), 76.
#' }



FNLM<-function (ce, qe) {
  x <- ce
  y <- qe
  data <- data.frame(x, y)

  fit1 <- y ~ (kf * (x^(1/n)))
  start1 <- expand.grid(kf = seq(0.1, 100, length.out = 20), n = seq(0.1, 10, length.out = 20))

  suppressWarnings(fit2 <- nls2::nls2(fit1, start = start1, data = data,
                                      control = nls.control(maxiter = 100, warnOnly = TRUE), algorithm = "port"))

  qe.fitted <-predict(fit2)

  c <- summary(fit2)
  kf <- c$coefficients[1]
  n <- c$coefficients[2]

  aic <- AIC(fit2)
  aicc <- AICcmodavg::AICc(fit2)
  bic <- BIC(fit2)


  rmse <-sqrt(mean((qe - qe.fitted)^2))
  mse <- mean((qe - qe.fitted)^2)
  mae <- mean(abs(qe - qe.fitted))
  mape <- (mean(abs(qe - qe.fitted)/qe))*100
  chi.sq <- sum(((qe - qe.fitted)^2)/qe.fitted)
  correl <- cor.test(ce, qe)
  mpsd <- (sqrt((sum(((qe.fitted - qe)/qe)^2))/(length(x) - 2)))*100


  n.fit <- function(x) {
    (kf * (x^(1/n)))
  }

  adsorption_plot <- ggplot(data, aes(x = x, y = y)) + geom_point(color = "blue") +
    geom_function(color = "red", fun = n.fit, lwd =1) +
    labs(x = "ce", y = "qe", title = "Freundlich Isotherm Nonlinear Model") +
    theme(plot.title = element_text(hjust = 0.5))

  output <- list("Freundlich Isotherm Nonlinear Model"= summary(fit2),
                 'correlation (ce, qe)' = correl,
                 kf = kf,
                 '1/n' = 1/n,
                 AIC = aic,
                 AICc = aicc,
                 BIC = bic,
                 RMSE = rmse,
                 MSE = mse,
                 MAE = mae,
                 'MAPE (%)' = mape,
                 'MPSD (%)' = mpsd,
                 Chi.square = chi.sq,
                 Plot =  adsorption_plot)


  return(output)
}












#' @title Langmuir Linear Model

#' @description This model will fit the adsorption data to the linear form of the Langmuir equation and will give the estimates of the Langmuir parameters, namely "b" and "k" while evaluating the performance efficiency of the linear model of Langmuir through several error functions.

#' @param ce Equilibrium concentration of the adsorbate in the solution

#' @param qe Amount adsorbed
#' @import AICcmodavg ggplot2 stats
#' @return
#' \itemize{
#'   \item Langmuir Isotherm Linear Model: Model summary
#'   \item correlation (ce, qe): Correlation between ce and qe
#'   \item b: Adsorption maxima
#'   \item k: Langmuir adsorption constant related to bonding energy
#'   \item AIC: Akaike information criterion
#'   \item AICc: Corrected Akaike information criterion
#'   \item BIC: Bayesian information criterion
#'   \item RMSE: Root Mean Squared Error
#'   \item MSE: Mean Squared Error
#'   \item MAE: Mean Absolute Error
#'   \item MAPE: Mean Absolute Percentage Error
#'   \item Chi.square: Chi-square value
#' }

#' @export

#' @usage LLM(ce, qe)


#' @examples
#' ce <- c(0.025, 0.04, 0.055, 0.099, 0.139, 0.402, 1.999, 11.336)
#' qe <- c(17.21, 35.42, 51.238, 72.659, 89.268, 182.21, 345.29, 634.231)
#' m.fit <- LLM (ce, qe)



#' @references
#' \itemize{
#' \item Langmuir, I. (1918). The adsorption of gases on plane surfaces of glass, mica and platinum. Journal of the American Chemical society, 40(9), 1361-1403.


#' \item Datta, S. P., Bhadoria, P. B. S., & Kar, S. (1998). Availability of extractable boron in some acid soils, West Bengal, India. Communications in soil science and plant analysis, 29(15-16), 2285-2306.

#' \item Roy, A., Manjaiah, K. M., Datta, S. P., Rakshit, D., Barman, M., Ray, P., Golui, D., Raza, M. B., Tigga, P., Mondal, S., Vishwanath, Meena, S., & Meena, P. (2025). Effect of Low-Molecular-Weight Organic Acids and Silicon on Arsenic Adsorption and Desorption in a Paddy Soil of Bengal Delta Plain: Insights from Thermodynamics and Equilibrium Modeling. Water, Air, & Soil Pollution, 236(6), 344.
#' \item Raza, M. B., Datta, S. P., Golui, D., Barman, M., Ray, P., Upadhyay, D., Mishra, R., Roy, A., & Dash, A. K. (2025). Enhancing soil arsenic immobilization with organic and inorganic amendments: Insights from sorption–desorption study. Environmental Monitoring and Assessment, 197(1), 76.
#' }




LLM<-function (ce, qe) {
  x <- ce
  y <- ce/qe
  data <- data.frame(x, y)

  fit <- lm(y ~ x)

  fitted <- fit$fitted.values
  qe.fitted <- x/fitted


  c <- summary(fit)
  a <- c$coefficients[1]
  b <- c$coefficients[2]

  b <- 1/b

  k <- 1/(a*b)


  aic <- AIC(fit)
  aicc <- AICcmodavg::AICc(fit)
  bic <- BIC(fit)


  rmse <-sqrt(mean((qe - qe.fitted)^2))
  mse <- mean((qe - qe.fitted)^2)
  mae <- mean(abs(qe - qe.fitted))
  mape <- (mean(abs(qe - qe.fitted)/qe))*100
  chi.sq <- sum(((qe - qe.fitted)^2)/qe.fitted)
  correl <- cor.test(ce, qe)

  data <- rbind.data.frame(c(0,0), data)

  adsorption_plot <- ggplot(data, aes(x = x, y = y)) + geom_point(color = "blue") +
    geom_smooth(formula = y ~ x, method = "lm", se = F, color = "red") +
    labs(x = "ce", y = "ce/qe", title = "Langmuir Isotherm Linear Model") +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

  output <- list("Langmuir Isotherm Linear Model"= summary(fit),
                 'correlation (ce, qe)' = correl,
                 b = b,
                 k = k,
                 AIC = aic,
                 AICc = aicc,
                 BIC = bic,
                 RMSE = rmse,
                 MSE = mse,
                 MAE = mae,
                 'MAPE (%)' = mape,
                 Chi.square = chi.sq,
                 Plot = adsorption_plot)

  return(output)

}















#' @title Langmuir Nonlinear Model

#' @description This model will fit the adsorption data to the nonlinear form of the Langmuir equation and will give the estimates of the Langmuir parameters, namely "b" and "k" while evaluating the performance efficiency of the nonlinear model of Langmuir through several error functions.

#' @param ce Equilibrium concentration of the adsorbate in the solution

#' @param qe Amount adsorbed
#' @import AICcmodavg ggplot2 nls2 stats
#' @return
#' \itemize{
#'   \item Langmuir Isotherm Nonlinear Model: Model summary
#'   \item correlation (ce, qe): Correlation between ce and qe
#'   \item b: Adsorption maxima
#'   \item k: Langmuir adsorption constant related to bonding energy
#'   \item AIC: Akaike information criterion
#'   \item AICc: Corrected Akaike information criterion
#'   \item BIC: Bayesian information criterion
#'   \item RMSE: Root Mean Squared Error
#'   \item MSE: Mean Squared Error
#'   \item MAE: Mean Absolute Error
#'   \item MAPE: Mean Absolute Percentage Error
#'   \item MPSD: Marquardt’s Percent Standard Deviation
#'   \item Chi.square: Chi-square value
#' }

#' @export
#' @usage LNLM (ce, qe)

#' @examples
#' ce <- c(0.025, 0.04, 0.055, 0.099, 0.139, 0.402, 1.999, 11.336)
#' qe <- c(17.21, 35.42, 51.238, 72.659, 89.268, 182.21, 345.29, 634.231)
#' m.fit <- LNLM (ce, qe)



#' @references
#' \itemize{
#' \item Langmuir, I. (1918). The adsorption of gases on plane surfaces of glass, mica and platinum. Journal of the American Chemical society, 40(9), 1361-1403.

#' \item Datta, S. P., Bhadoria, P. B. S., & Kar, S. (1998). Availability of extractable boron in some acid soils, West Bengal, India. Communications in soil science and plant analysis, 29(15-16), 2285-2306.

#' \item Roy, A., Manjaiah, K. M., Datta, S. P., Rakshit, D., Barman, M., Ray, P., Golui, D., Raza, M. B., Tigga, P., Mondal, S., Vishwanath, Meena, S., & Meena, P. (2025). Effect of Low-Molecular-Weight Organic Acids and Silicon on Arsenic Adsorption and Desorption in a Paddy Soil of Bengal Delta Plain: Insights from Thermodynamics and Equilibrium Modeling. Water, Air, & Soil Pollution, 236(6), 344.
#' \item Raza, M. B., Datta, S. P., Golui, D., Barman, M., Ray, P., Upadhyay, D., Mishra, R., Roy, A., & Dash, A. K. (2025). Enhancing soil arsenic immobilization with organic and inorganic amendments: Insights from sorption–desorption study. Environmental Monitoring and Assessment, 197(1), 76.
#' }



LNLM<-function (ce, qe) {
  x <- ce
  y <- qe
  data <- data.frame(x, y)

  fit1 <- y ~ (k * b * x)/(1 + (k * x))
  N <- nrow(na.omit(data))

  start1 <- expand.grid(b = seq(0.1, 100, length.out = 20), k = seq(0.1, 10, length.out = 20))

  suppressWarnings(fit2 <- nls2::nls2(fit1, start = start1,
                                      data = data, control = nls.control(maxiter = 100, warnOnly = TRUE),
                                      algorithm = "port"))


  qe.fitted <-predict(fit2)

  c <- summary(fit2)
  b.est <- c$coefficients[1]
  k.est <- c$coefficients[2]

  aic <- AIC(fit2)
  aicc <- AICcmodavg::AICc(fit2)
  bic <- BIC(fit2)


  rmse <-sqrt(mean((qe - qe.fitted)^2))
  mse <- mean((qe - qe.fitted)^2)
  mae <- mean(abs(qe - qe.fitted))
  mape <- (mean(abs(qe - qe.fitted)/qe))*100
  chi.sq <- sum(((qe - qe.fitted)^2)/qe.fitted)
  correl <- cor.test(ce, qe)
  mpsd <- (sqrt((sum(((qe.fitted - qe)/qe)^2))/(length(x) - 2)))*100

  data <- rbind.data.frame(c(0,0), data)

  n.fit <- function(x) {
    ((k.est * b.est * x)/(1 + (k.est * x)))
  }

  adsorption_plot <- ggplot(data, aes(x = x, y = y)) + geom_point(color = "blue") +
    geom_function(color = "red", fun = n.fit, lwd =1) +
    labs(x = "ce", y = "qe", title = "Langmuir Isotherm Nonlinear Model") +
    theme(plot.title = element_text(hjust = 0.5))

  output <- list("Langmuir Isotherm Nonlinear Model"= summary(fit2),
                 'correlation (ce, qe)' = correl,
                 b = b.est,
                 k = k.est,
                 AIC = aic,
                 AICc = aicc,
                 BIC = bic,
                 RMSE = rmse,
                 MSE = mse,
                 MAE = mae,
                 'MAPE (%)' = mape,
                 'MPSD (%)'= mpsd,
                 Chi.square = chi.sq,
                 Plot = adsorption_plot)

  return(output)
}


