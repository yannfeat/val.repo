#' Example Data to compute AMIM
#' @name exampledata
#' @format ## `exampledata`
#' A data datatable with the following columns:
#' \describe{
#'   \item{Date}{Date format YYYY-MM-DD}
#'   \item{ticker}{Imaginary ticker}
#'   \item{RET}{Imaginary return}
#'   ...
#' }
#' @source Vu Le Tran
"exampledata"

#' Confidence Interval Data to compute AMIM
#' @name CI
#' @format ## `CI`
#' A data datatable with the following columns:
#' \describe{
#'   \item{N}{Number of lags}
#'   \item{a}{Scale parameter equal to 1 as in Tran & Leivrik (2019)}
#'   \item{CI}{Confidence interval accordingly
#'      each number lags and scale parameter}
#'   ...
#' }
#' @source Tran & Leivrik (2019)
#'
"CI"
