# Modified isotherm models to print named outputs and model summary directly

# Imports and global variables
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth labs scale_color_manual theme_bw geom_line
#' @importFrom stats lm coef predict
#' @importFrom stats AIC nls nls.control resid

# Avoid global variable notes for ggplot2 aesthetics
utils::globalVariables(c("Actual", "Predicted"))

#' Langmuir Isotherm Model
#'
#' Langmuir isotherm assumes monolayer adsorption onto a surface with a finite number of identical sites 
#' (Langmuir, 1918). It is characterized by uniform energies of adsorption onto the surface and 
#' no transmigration of adsorbate in the plane of the surface. The model is described by the equation:
#'   Q = (Qmax * KL * Ce) / (1 + KL * Ce)
#' where Q is the amount adsorbed, Ce is the equilibrium concentration, Qmax is the maximum adsorption capacity,
#' and KL is the Langmuir constant.
#' https://doi.org/10.1021/ja02242a004 
#'
#' @param Ce Numeric vector of equilibrium concentrations.
#' @param Qe Numeric vector of amount adsorbed.
#' @return A named list of Langmuir parameters and model details.
#' @export
#' @family linear
#' @examples
#' Ce <- c(1, 2, 3, 4, 5)
#' Qe <- c(0.8, 1.5, 2.1, 2.6, 2.9)
#' result <- langmuir_model(Ce, Qe)
#' print(result[1:2])
#' print(result$`Model Summary`)
#' print(result$Plot)
langmuir_model <- function(Ce, Qe) {
  stopifnot(length(Ce) == length(Qe))
  Ce_by_Qe <- Ce / Qe
  model <- lm(Ce_by_Qe ~ Ce)
  slope <- coef(model)[2]
  intercept <- coef(model)[1]
  Qmax <- 1 / slope
  KL <- slope / intercept
  predicted <- predict(model)
  plot_data <- data.frame(Ce, Actual = Ce_by_Qe, Predicted = predicted)
  p <- ggplot(plot_data, aes(x = Ce)) +
    geom_point(aes(y = Actual, color = "Actual"), size = 3) +
    geom_point(aes(y = Predicted, color = "Predicted"), size = 3) +
    geom_smooth(aes(y = Actual), method = "lm", linetype = "dotted", color = "black") +
    labs(title = "Langmuir Adsorption Isotherm", x = expression(C[e]), y = expression(C[e]/Q[e])) +
    scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
    theme_bw(base_size = 14)
  list(
    "Langmuir Qmax (mg/g)" = Qmax,
    "Langmuir KL (L/mg)" = KL,
    "Model Summary" = summary(model),
    "Plot" = p
  )
}

#' Freundlich Isotherm Model
#'
#' Freundlich isotherm describes adsorption on heterogeneous surfaces and assumes that the stronger binding 
#' sites are occupied first (Freundlich, 1907). It is represented by:
#'   Q = Kf * Ce^(1/n)
#' where Kf is the Freundlich constant related to adsorption capacity and n indicates adsorption intensity.
#'  https://doi.org/10.1002/ange.19070201805 
#'
#' @param Ce Numeric vector of equilibrium concentrations.
#' @param Qe Numeric vector of amount adsorbed.
#' @return A named list of Freundlich parameters and model details.
#' @export
#' @family linear
#' @examples
#' Ce <- c(1, 2, 3, 4, 5)
#' Qe <- c(0.8, 1.5, 2.1, 2.6, 2.9)
#' result <- freundlich_model(Ce, Qe)
#' print(result[1:2])
#' print(result$`Model Summary`)
#' print(result$Plot)
freundlich_model <- function(Ce, Qe) {
  stopifnot(length(Ce) == length(Qe))
  data <- subset(data.frame(Ce, Qe), Ce > 0 & Qe > 0)
  log_Ce <- log10(data$Ce)
  log_Qe <- log10(data$Qe)
  model <- lm(log_Qe ~ log_Ce)
  intercept <- coef(model)[1]
  slope <- coef(model)[2]
  Kf <- 10^intercept
  n <- 1 / slope
  predicted <- predict(model)
  plot_data <- data.frame(log_Ce, Actual = log_Qe, Predicted = predicted)
  p <- ggplot(plot_data, aes(x = log_Ce)) +
    geom_point(aes(y = Actual, color = "Actual"), size = 3) +
    geom_point(aes(y = Predicted, color = "Predicted"), size = 3) +
    geom_smooth(aes(y = Actual), method = "lm", linetype = "dotted", color = "black") +
    labs(title = "Freundlich Adsorption Isotherm", x = expression(logC[e]), y = expression(logQ[e])) +
    scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
    theme_bw(base_size = 14)
  list(
    "Freundlich Kf" = Kf,
    "Freundlich n" = n,
    "Model Summary" = summary(model),
    "Plot" = p
  )
}

#' BET Isotherm Model
#'
#' The BET isotherm extends the Langmuir theory to multilayer adsorption (Brunauer et al., 1938). It is used 
#' to estimate surface area and porosity of adsorbents. The model is applicable under specific physical or 
#' chemical conditions and is given by:
#'   Q = (Qm * Cb * P) / ((P0 - P)(1 + (Cb - 1) * P / P0))
#'   https://doi.org/10.1021/ja01269a023 
#'
#' @param Ce Numeric vector of equilibrium concentrations.
#' @param Qe Numeric vector of amount adsorbed.
#' @param Cs Saturation concentration.
#' @return A named list of BET parameters and model details.
#' @export
#' @family linear
#' @examples
#' Ce <- c(1, 2, 3, 4, 5)
#' Qe <- c(0.8, 1.5, 2.1, 2.6, 2.9)
#' result <- bet_model(Ce, Qe)
#' print(result[1:2])
#' print(result$`Model Summary`)
#' print(result$Plot)
bet_model <- function(Ce, Qe, Cs = max(Ce) * 1.1) {
  stopifnot(length(Ce) == length(Qe))
  data <- subset(data.frame(Ce, Qe), Ce < Cs & Qe > 0)
  BET_y <- data$Ce / (data$Qe * (Cs - data$Ce))
  BET_x <- data$Ce / Cs
  model <- lm(BET_y ~ BET_x)
  slope <- coef(model)[2]
  intercept <- coef(model)[1]
  Qm <- 1 / (slope + intercept)
  Cb <- 1 + (slope / intercept)
  predicted <- predict(model)
  plot_data <- data.frame(BET_x, Actual = BET_y, Predicted = predicted)
  p <- ggplot(plot_data, aes(x = BET_x)) +
    geom_point(aes(y = Actual, color = "Actual"), size = 3) +
    geom_point(aes(y = Predicted, color = "Predicted"), size = 3) +
    geom_smooth(aes(y = Actual), method = "lm", linetype = "dotted", color = "black") +
    labs(title = "BET Adsorption Isotherm", x = expression(C[e]/C[s]), y = expression(C[e]/(Q[e]*(C[s] - C[e])))) +
    scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
    theme_bw(base_size = 14)
  list(
    "BET Qm (mg/g)" = Qm,
    "BET Cb" = Cb,
    "Model Summary" = summary(model),
    "Plot" = p
  )
}

#' Temkin Isotherm Model
#'
#' The Temkin isotherm considers the effects of indirect adsorbate/adsorbate interactions. It assumes that 
#' the heat of adsorption of all molecules in the layer decreases linearly with coverage (Temkin and Pyzhev 1940).
#' The model is given by:
#'   Q = (RT / bT) * ln(AT * Ce)
#'
#' @param Ce Numeric vector of equilibrium concentrations.
#' @param Qe Numeric vector of amount adsorbed.
#' @param R Universal gas constant.
#' @param T Temperature in Kelvin.
#' @return A named list of Temkin parameters and model details.
#' @export
#' @family linear
#' @examples
#' Ce <- c(1, 2, 3, 4, 5)
#' Qe <- c(0.8, 1.5, 2.1, 2.6, 2.9)
#' result <- temkin_model(Ce, Qe)
#' print(result[1:2])
#' print(result$`Model Summary`)
#' print(result$Plot)
temkin_model <- function(Ce, Qe, R = 8.314, T = 298) {
  stopifnot(length(Ce) == length(Qe))
  data <- subset(data.frame(Ce, Qe), Ce > 0 & Qe > 0)
  ln_Ce <- log(data$Ce)
  adjusted_Qe <- data$Qe * R * T
  model <- lm(adjusted_Qe ~ ln_Ce)
  slope <- coef(model)[2]
  intercept <- coef(model)[1]
  B <- slope / (R * T)
  A <- exp(intercept / slope)
  predicted <- predict(model)
  plot_data <- data.frame(ln_Ce, Actual = adjusted_Qe, Predicted = predicted)
  p <- ggplot(plot_data, aes(x = ln_Ce)) +
    geom_point(aes(y = Actual, color = "Actual"), size = 3) +
    geom_point(aes(y = Predicted, color = "Predicted"), size = 3) +
    geom_smooth(aes(y = Actual), method = "lm", linetype = "dotted", color = "black") +
    labs(title = "Temkin Adsorption Isotherm", x = expression(ln(C[e])), y = expression(Q[e] * R * T)) +
    scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
    theme_bw(base_size = 14)
  list(
    "Temkin A" = A,
    "Temkin B" = B,
    "Model Summary" = summary(model),
    "Plot" = p
  )
}


#' Non-linear Langmuir Model
#'
#' Fits the Langmuir isotherm model using non-linear least squares (nls).
#'
#' @param Ce Numeric vector of equilibrium concentrations.
#' @param Qe Numeric vector of amount adsorbed.
#' @return A named list of Langmuir parameters and model details.
#' @export
#' @family nonlinear
#' @examples
#' Ce <- c(1, 2, 4, 6, 8, 10)
#' Qe <- c(0.9, 1.6, 2.3, 2.7, 2.9, 3.0)
#' result <- nonlinear_langmuir(Ce, Qe)
#' print(result$`Langmuir Qmax (mg/g)`)
#' print(result$`Langmuir KL (L/mg)`)
#' print(result$AIC)
#' print(result$`Pseudo R2`)
#' print(result$Plot)

nonlinear_langmuir <- function(Ce, Qe) {
  stopifnot(length(Ce) == length(Qe))
  df <- data.frame(Ce, Qe)
  model <- nls(Qe ~ (Qmax * KL * Ce) / (1 + KL * Ce),
               data = df,
               start = list(Qmax = max(Qe), KL = 0.1))
  coefs <- coef(model)
  predicted <- predict(model)
  residuals <- resid(model)
  pseudo_r2 <- 1 - sum(residuals^2) / sum((Qe - mean(Qe))^2)
  aic_value <- AIC(model)
  
  plot_data <- data.frame(Ce, Actual = Qe, Predicted = predicted)
  p <- ggplot(plot_data, aes(x = Ce)) +
    geom_point(aes(y = Actual, color = "Actual"), size = 3) +
    geom_point(aes(y = Predicted, color = "Predicted"), size = 3) +
    geom_smooth(aes(y = Predicted), method = "loess", se = FALSE, color = "black", linetype = "dotted") +
    labs(title = "Langmuir Adsorption Isotherm (Non-linear)", x = "Ce", y = "Qe") +
    scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
    theme_bw(base_size = 14)
  
  list(
    "AIC" = aic_value,
    "Pseudo R2" = pseudo_r2,
    "Langmuir Qmax (mg/g)" = coefs["Qmax"],
    "Langmuir KL (L/mg)" = coefs["KL"],
    "Model Summary" = summary(model),
    "Plot" = p
  )
}

#' Non-linear Freundlich Model
#'
#' @param Ce Numeric vector of equilibrium concentrations.
#' @param Qe Numeric vector of amount adsorbed.
#' @return A named list of Freundlich parameters and model details.
#' @export
#' @family nonlinear
#' @examples
#' Ce <- c(0.5, 1, 2, 4, 6, 8)
#' Qe <- c(0.3, 0.8, 1.6, 2.4, 2.9, 3.2)
#' result <- nonlinear_freundlich(Ce, Qe)
#' print(result$`Freundlich Kf`)
#' print(result$`Freundlich n`)
#' print(result$AIC)
#' print(result$`Pseudo R2`)
#' print(result$Plot)

nonlinear_freundlich <- function(Ce, Qe) {
  stopifnot(length(Ce) == length(Qe))
  df <- data.frame(Ce, Qe)
  model <- nls(Qe ~ Kf * Ce^(1/n),
               data = df,
               start = list(Kf = 1, n = 2))
  coefs <- coef(model)
  predicted <- predict(model)
  residuals <- resid(model)
  pseudo_r2 <- 1 - sum(residuals^2) / sum((Qe - mean(Qe))^2)
  aic_value <- AIC(model)
  
  plot_data <- data.frame(Ce, Actual = Qe, Predicted = predicted)
  p <- ggplot(plot_data, aes(x = Ce)) +
    geom_point(aes(y = Actual, color = "Actual"), size = 3) +
    geom_point(aes(y = Predicted, color = "Predicted"), size = 3) +
    geom_smooth(aes(y = Predicted), method = "loess", se = FALSE, color = "black", linetype = "dotted") +
    labs(title = "Freundlich Adsorption Isotherm (Non-linear)", x = "Ce", y = "Qe") +
    scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
    theme_bw(base_size = 14)
  
  list(
    "AIC" = aic_value,
    "Pseudo R2" = pseudo_r2,
    "Freundlich Kf" = coefs["Kf"],
    "Freundlich n" = coefs["n"],
    "Model Summary" = summary(model),
    "Plot" = p
  )
}


#' Non-linear BET Model
#'
#' @param Ce Numeric vector of equilibrium concentrations.
#' @param Qe Numeric vector of amount adsorbed.
#' @param Cs Saturation concentration.
#' @return A named list of BET parameters and model details.
#' @export
#' @family nonlinear
#' @examples
#' Ce <- c(1, 2.5, 4, 5.5, 7)
#' Qe <- c(0.4, 1.0, 1.7, 2.3, 2.7)
#' result <- nonlinear_bet(Ce, Qe)
#' print(result$`BET Qm (mg/g)`)
#' print(result$`BET Cb`)
#' print(result$AIC)
#' print(result$`Pseudo R2`)
#' print(result$Plot)

nonlinear_bet <- function(Ce, Qe, Cs = max(Ce) * 1.1) {
  stopifnot(length(Ce) == length(Qe))
  df <- data.frame(Ce, Qe)
  df <- subset(df, Ce < Cs & Qe > 0)
  
  model <- nls(Qe ~ (Qm * Cb * Ce) / ((Cs - Ce) * (1 + (Cb - 1) * Ce / Cs)),
               data = df,
               start = list(Qm = max(Qe), Cb = 1.1),
               control = nls.control(minFactor = 1e-10, warnOnly = TRUE))
  coefs <- coef(model)
  predicted <- predict(model)
  residuals <- resid(model)
  pseudo_r2 <- 1 - sum(residuals^2) / sum((df$Qe - mean(df$Qe))^2)
  aic_value <- AIC(model)
  
  plot_data <- data.frame(Ce = df$Ce, Actual = df$Qe, Predicted = predicted)
  p <- ggplot(plot_data, aes(x = Ce)) +
    geom_point(aes(y = Actual, color = "Actual"), size = 3) +
    geom_point(aes(y = Predicted, color = "Predicted"), size = 3) +
    geom_smooth(aes(y = Predicted), method = "loess", se = FALSE, color = "black", linetype = "dotted") +
    labs(title = "BET Adsorption Isotherm (Non-linear)", x = "Ce", y = "Qe") +
    scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
    theme_bw(base_size = 14)
  
  list(
    "AIC" = aic_value,
    "Pseudo R2" = pseudo_r2,
    "BET Qm (mg/g)" = coefs["Qm"],
    "BET Cb" = coefs["Cb"],
    "Model Summary" = summary(model),
    "Plot" = p
  )
}

#' Non-linear Temkin Model
#'
#' @param Ce Numeric vector of equilibrium concentrations.
#' @param Qe Numeric vector of amount adsorbed.
#' @param R Universal gas constant.
#' @param T Temperature in Kelvin.
#' @return A named list of Temkin parameters and model details.
#' @export
#' @family nonlinear
#' @examples
#' Ce <- c(0.5, 1.5, 3, 4.5, 6)
#' Qe <- c(0.7, 1.3, 2.0, 2.4, 2.7)
#' result <- nonlinear_temkin(Ce, Qe)
#' print(result$`Temkin A`)
#' print(result$`Temkin B`)
#' print(result$AIC)
#' print(result$`Pseudo R2`)
#' print(result$Plot)

nonlinear_temkin <- function(Ce, Qe, R = 8.314, T = 298) {
  stopifnot(length(Ce) == length(Qe))
  df <- data.frame(Ce, Qe)
  model <- nls(Qe ~ (R * T / bT) * log(AT * Ce),
               data = df,
               start = list(AT = 1, bT = 1))
  coefs <- coef(model)
  predicted <- predict(model)
  residuals <- resid(model)
  pseudo_r2 <- 1 - sum(residuals^2) / sum((Qe - mean(Qe))^2)
  aic_value <- AIC(model)
  
  plot_data <- data.frame(Ce, Actual = Qe, Predicted = predicted)
  p <- ggplot(plot_data, aes(x = Ce)) +
    geom_point(aes(y = Actual, color = "Actual"), size = 3) +
    geom_point(aes(y = Predicted, color = "Predicted"), size = 3) +
    geom_smooth(aes(y = Predicted), method = "loess", se = FALSE, color = "black", linetype = "dotted") +
    labs(title = "Temkin Adsorption Isotherm (Non-linear)", x = "Ce", y = "Qe") +
    scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
    theme_bw(base_size = 14)
  
  list(
    "AIC" = aic_value,
    "Pseudo R2" = pseudo_r2,
    "Temkin A" = coefs["AT"],
    "Temkin B" = R * T / coefs["bT"],
    "Model Summary" = summary(model),
    "Plot" = p
  )
}