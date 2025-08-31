#' @title Cross-Validation for Langmuir Isotherm Second Linear Model with Clustering-based Fold Assignment
#' @name cv_langmuirLM2
#' @description
#' Performs K-fold or leave-one-out cross-validation (LOOCV) on the linearized Langmuir isotherm model (Form 2):
#' 1 / Qe = (1 / (Qmax * b)) * (1 / Ce) + 1 / Qmax
#' This corresponds to a linear regression of 1 / Qe versus 1 / Ce.
#' Evaluates predictive performance using Mean Squared Error (MSE). Optionally displays a barplot of fold-wise MSEs.
#' Optionally uses clustering-based fold assignment to preserve data structure.
#'
#' @param Ce Numeric vector of equilibrium concentrations (Ce). Must be positive.
#' @param Qe Numeric vector of amounts adsorbed (Qe). Must be positive and same length as Ce.
#' @param K Integer. Number of folds to use in K-fold CV (default is 10). Ignored if loocv = TRUE.
#' @param seed Integer. Random seed for reproducibility (default is 123).
#' @param loocv Logical. If TRUE, performs leave-one-out cross-validation (overrides K).
#' @param plot Logical. If TRUE, displays a barplot of fold MSEs (default is FALSE).
#' @param use_clustering Logical. If TRUE, assigns folds using k-means clustering on the data (default FALSE).
#'
#' @return A list with the following components:
#' \describe{
#'   \item{mean_mse}{The average mean squared error across all folds.}
#'   \item{fold_mse}{A numeric vector of MSEs for each fold.}
#' }
#'
#' @examples
#' Ce <- c(0.01353, 0.04648, 0.13239, 0.27714, 0.41600, 0.63607, 0.80435, 1.10327, 1.58223)
#' Qe <- c(0.03409, 0.06025, 0.10622, 0.12842, 0.15299, 0.15379, 0.15735, 0.15735, 0.16607)
#' cv_langmuirLM2(Ce, Qe, K = 5, seed = 123, plot = TRUE, use_clustering = TRUE)
#' cv_langmuirLM2(Ce, Qe, loocv = TRUE, plot = TRUE)
#'
#' @author Paul Angelo C. Manlapaz
#'
#' @references Montgomery, D.C., Peck, E.A., & Vining, G.G. (2012).
#' Introduction to Linear Regression Analysis, 5th ed. Wiley.
#'
#' @import stats
#' @import graphics
#'
#' @importFrom stats lm predict kmeans
#' @importFrom graphics abline barplot legend
#' @export

utils::globalVariables(c("invCe", "invQe"))

cv_langmuirLM2 <- function(Ce, Qe, K = 10, seed = 123, loocv = FALSE, plot = FALSE, use_clustering = FALSE) {
  if (length(Ce) != length(Qe)) {
    stop("Ce and Qe must be of the same length.")
  }
  if (any(Ce <= 0) || any(Qe <= 0)) {
    stop("All Ce and Qe values must be positive for reciprocal transformation.")
  }

  n <- length(Ce)
  if (loocv) {
    K <- n
  } else if (K > n) {
    stop(sprintf("Number of folds K (%d) cannot exceed the number of observations (%d).", K, n))
  }

  set.seed(seed)
  data <- data.frame(invQe = 1 / Qe, invCe = 1 / Ce)

  if (use_clustering && !loocv) {
    clusters <- stats::kmeans(data, centers = K, nstart = 25)$cluster
    folds <- integer(n)
    for (cl in unique(clusters)) {
      idx <- which(clusters == cl)
      n_cl <- length(idx)
      folds[idx] <- sample(rep(1:K, length.out = n_cl))
    }
  } else {
    folds <- sample(rep(1:K, length.out = n))
  }

  fold_mse <- numeric(K)

  for (i in 1:K) {
    train_data <- data[folds != i, ]
    test_data  <- data[folds == i, ]

    model <- tryCatch({
      stats::lm(invQe ~ invCe, data = train_data)
    }, error = function(e) {
      warning("Model fitting failed for fold ", i, ": ", e$message)
      return(NULL)
    })

    if (is.null(model)) {
      fold_mse[i] <- NA
      next
    }

    preds <- stats::predict(model, newdata = test_data)
    fold_mse[i] <- mean((test_data$invQe - preds)^2)
  }

  mean_mse <- mean(fold_mse, na.rm = TRUE)

  if (plot) {
    barplot(
      fold_mse,
      names.arg = paste("Fold", 1:K),
      col = "skyblue",
      main = "MSE per Fold",
      ylab = "Mean Squared Error"
    )
    abline(h = mean_mse, col = "red", lty = 2)
    legend("topright", legend = sprintf("Mean MSE = %.4f", mean_mse),
           col = "red", lty = 2, bty = "n")
  }

  return(list(
    mean_mse = mean_mse,
    fold_mse = fold_mse
  ))
}
