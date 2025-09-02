#' Predict method for ad Objects
#'
#' @param object An object of class \code{ad}, created by the function \code{ad}.
#' @param newdata A data frame or matrix containing new data.
#' @param type One of 'class' (for class prediction) or 'prob' (for probabilities).
#' @param na.action A function specifying the action to be taken if NAs are
#' found; default is to predict NA (na.pass).
#' @param ... Currently not used.
#'
#' @return A vector of predicted values.
#'
#' @details Specifying 'class' for \code{type} returns the class of each
#' observation as anomalous or non-anomalous. Specifying 'prob' returns the
#' probability of each observation.
#'
#' @examples
#'
#' x1 <- c(1,.2,3,1,1,.7,-2,-1)
#' x2 <- c(0,.5,0,.4,0,1,-.3,-.1)
#' x <- do.call(cbind,list(x1,x2))
#' y <- c(0,0,0,0,0,0,1,1)
#' dframe <- data.frame(x,y)
#' df_fit <- ad(y ~ x1 + x2, dframe)
#' predict(df_fit, newdata = dframe)
#'
#'@importFrom stats delete.response model.frame na.pass
#'
#'
#'@export
predict.ad <- function(object, newdata, type = 'class', na.action = na.pass, ...) {
  if (!inherits(object, "ad"))
    stop("object not of class ad.")

  if (!(type %in% c('class','prob'))){
    stop("type must be either 'class' or 'prob'.")
  }

  if (inherits(object, "ad.formula")) {
    newdata <- as.data.frame(newdata)
    rn <- row.names(newdata)
    Terms <- delete.response(object$terms)
    x <- model.frame(Terms, newdata, na.action = na.action)
  } else { #must inherit from ad.default
    if (is.null(dim(newdata)))
      dim(newdata) <- c(1, length(newdata))
    x <- newdata
    if (nrow(x) == 0)
      stop("newdata has 0 rows.")
    if (any(is.na(x)))
      stop("newdata contains missing values.")
  }

  epsilon <- object$epsilon
  train_mean <- object$train_mean
  train_var <- object$train_var


  #prediction
  if (object$univariate == TRUE) {
    newdata_probs_prod <- .univariate_pdf(x,train_mean,train_var)
  } else {
    newdata_probs_prod <- .multivariate_pdf(x,train_mean,train_var)
  }
  if (type == 'class') {
    predictions <- as.numeric(newdata_probs_prod < epsilon)
    return(predictions)
  } else if (type == 'prob') {
    return(newdata_probs_prod)
  }
}
