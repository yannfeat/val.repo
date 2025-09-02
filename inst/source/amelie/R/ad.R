#' ad: anomaly detection with normal probability density functions.
#'
#' @param formula An object of class "formula": a symbolic description of the
#' model to be fitted.
#' @param data A data frame containing the features (predictors) and target.
#' @param x A matrix of numeric features.
#' @param y A vector of numeric target values, either 0 or 1, with 1
#' assumed to be anomalous.
#' @param univariate Logical indicating whether the univariate pdf should be used.
#' @param score String indicating which score to use in optimization:
#' \code{f1} (default) or \code{mcc}.
#' @param steps Integer number of steps to take during epsilon optimization, default 1e3.
#' @param na.action A function specifying the action to be taken if NAs are
#' found.
#' @param ... Optional parameters to be passed to ad.default.
#'
#' @return An object of class \code{ad}:
#'   \item{call}{The original call to \code{ad}.}
#'   \item{univariate}{Logical indicating which pdf was computed.}
#'   \item{score}{The score that was used for optimization.}
#'   \item{epsilon}{The threshold value.}
#'   \item{train_mean}{Means of features in the training set.}
#'   \item{train_var}{Variances of features in the training set. If \code{univariate=FALSE}}, holds the covariance matrix for the features.
#'   \item{val_score}{The score obtained on the validation data set.
#'   0 to 1 for F1 score, -1 to 1 for Matthews correlation coefficient}
#'
#' @details
#'
#' \code{amelie} implements anomaly detection with normal probability
#' functions and maximum likelihood estimates.
#'
#' Features are assumed to be continuous, and the target is assumed to take
#' on values of \code{0} (negative case, no anomaly) or \code{1} (positive
#' case, anomaly).
#'
#' The threshold \code{epsilon} is optimized using the either the Matthews
#' correlation coefficient or F1 score.
#'
#' Variance and covariance are computed using \code{var} and \code{cov}, where
#' denominator \code{n-1} is used.
#'
#' Algorithm details are described in the Introduction vignette.
#'
#' The package follows the anomaly detection approach in Andrew Ng's course on
#' machine learning.
#'
#' @references
#' \href{https://www.coursera.org/learn/machine-learning}{Machine learning course}
#'
#' \href{https://en.wikipedia.org/wiki/Confusion_matrix}{Confusion matrix}
#'
#' \href{https://en.wikipedia.org/wiki/Matthews_correlation_coefficient}{Matthews correlation coefficient}
#'
#' @examples
#'
#' x1 <- c(1,.2,3,1,1,.7,-2,-1)
#' x2 <- c(0,.5,0,.4,0,1,-.3,-.1)
#' x <- do.call(cbind,list(x1,x2))
#' y <- c(0,0,0,0,0,0,1,1)
#' dframe <- data.frame(x,y)
#' df_fit <- ad(y ~ x1 + x2, dframe)
#' mat_fit <- ad(x = x, y = y)
#'
#'@importFrom stats sd cov


#'@export
ad <- function(x, ...){
  UseMethod("ad")
}

#'@rdname ad
#'@export
ad.formula <- function(formula, data, na.action = na.omit, ...) {
  call <- match.call()
  if (!inherits(formula, "formula"))
    stop("Method is only for formula objects.")

  if (!all(sapply(data,is.numeric))) {
    stop("Both x and y must be numeric.")
  }

  m <- match.call(expand.dots = FALSE)

  if (identical(class(eval.parent(m$data)), "matrix"))
    m$data <- as.data.frame(eval.parent(m$data))

  m$... <- NULL

  m[[1L]] <- quote(stats::model.frame)
  m$na.action <- na.action

  m <- eval(m, parent.frame())

  Terms <- attr(m, "terms")
  attr(Terms, "intercept") <- 0

  x <- model.matrix(Terms, m)
  y <- model.extract(m, "response")
  attr(x, "na.action") <- attr(y, "na.action") <- attr(m, "na.action")

  return_object <- ad.default(x, y, ...)
  return_object$call <- call
  return_object$call[[1]] <- as.name("ad")
  return_object$terms <- Terms
  if (!is.null(attr(m, "na.action")))
    return_object$na.action <- attr(m, "na.action")
  class(return_object) <- c("ad.formula", class(return_object))
  return (return_object)
}



#' @rdname ad
#' @export
ad.default <- function(x, y, univariate = TRUE,
                       score = 'f1',
                       steps = 1e3, ...) {

  # check score
  if (!score %in% c('f1','mcc')) stop ("score must be one of 'f1' or 'mcc'.")

  # check univariate
  if (!is.logical(univariate)) stop("univariate must be logical.")

  # check data
  .check_data(x,y)

  # split data into training and cross-validation sets
  split_obj <- .split_data(x,y)
  train_x <- split_obj$train_x
  # train_y <- split_obj$train_y #not used, and always expected to be 0
  val_x <- split_obj$val_x
  val_y <- split_obj$val_y


  # compute mean and variance of training set
  train_mean <- .mean2(train_x)

  # compute product of probabilities on training set
  if (univariate == TRUE) {
    train_var <- .var2(train_x)

    if (any(train_var == 0)) {
      stop("Feature with 0 variance found in training set. This results in Inf pdf values.")
    }


    # train_x_probs_prod <- .univariate_pdf(train_x,train_mean,train_var) #is this calculation necessary?
    val_x_probs_prod <- .univariate_pdf(val_x,train_mean,train_var)
  } else {
    train_var <- cov(train_x)

    # train_x_probs_prod <- .multivariate_pdf(train_x,train_mean,train_var)
    val_x_probs_prod <- .multivariate_pdf(val_x,train_mean,train_var)
  }
  # optimize epsilon using validation set
  epsilon <- .op_epsilon(val_x_probs_prod, val_y, score, steps)

  # compute predictions on training set
  val_predictions <- as.numeric(val_x_probs_prod < epsilon)

  # compute f1 score on validation set
  val_score <- .score(val_predictions, val_y, score)


  # create the return object
  call <- match.call()
  return_obj <- list(call = call,
                     univariate = univariate,
                     score = score,
                     steps = steps,
                     epsilon = epsilon,
                     train_mean = train_mean,
                     train_var = train_var,
                     # val_predictions = val_predictions,
                     val_score = val_score)
  class(return_obj) <- "ad"
  return(return_obj)
}


#' @rdname ad
#' @export
print.ad <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  cat("\n")
  cat("epsilon: ",x$epsilon,sep="")
}

