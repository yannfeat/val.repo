#' compute AICc
#'
#'
#' @param rss residual sum of squares
#' @param n number of observation
#' @param p number of nonzero parameters
#' @param type character "AICc"
#' @references \href{https://stats.stackexchange.com/questions/87345/calculating-aic-by-hand-in-r/}{Calculating AIC “by hand” in R in Stack Overflow}
#' @export
#' @examples
#' compute_aicc (rss=10, n=30, p=6, type = "AICc")
compute_aicc = function(rss, n, p, type = "AICc"){
  # rss is the residual sum of squares
  # n is number of observations
  # p is the number of nonzero parameters: coef + 1 (1 for sigma)
  if (type == "AICc") {
    scalor = 2
  }
  out = n*log(rss/n) + scalor*p*n/(n-p-1)
  return (out)
}



#' Expand the interaction terms for each subset group, say x11, x12, or c1, c2, c3
#'
#'
#' @param dat data frame
#' @param sel_names characters
#' @export
#' @examples
#' data("pringles_fat")
#' data_fat = pringles_fat
#' h_tmp = 1.3
#' x = data_fat[,c("c1","c2","c3","x11","x12","x21","x22")]
#' expand_interactions (dat=x, sel_names=c("c1", "c2", "c3"))

expand_interactions = function(dat, sel_names){
  # note: the first column is the response
  # we do not expand the response column

  dat = dat[,sel_names]
  names = colnames(dat) # equal to sel_names
  term = paste("~(", paste(names,collapse="+"),")^2-1")
  #term = paste("~(", term, ")^2-1")
  term = as.formula(term)
  term = model.matrix(term, dat)

  # dat_aug = model.matrix(as.formula(term), dat)
  # out = data.frame(dat_aug, dat[,ncol(dat)])
  # #colnames(out)[1] = colnames(dat)[1]
  # colnames(out)[ncol(out)] = colnames(dat)[ncol(dat)]
  out = term # return in format of matrix
  return (out)
}

# this utility function is taken from the R package bestsubset in GitHub
#' Create a list
#'
#' @param ... object to be included as elements in the list
#' @export
#' @examples
#' item = c(1:10)
#' enlist(item)
# enlist <- function (...)
# {
#   result <- list(...)
#   if ((nargs() == 1) & is.character(n <- result[[1]])) {
#     result <- as.list(seq(n))
#     names(result) <- n
#     for (i in n) result[[i]] <- get(i)
#   }
#   else {
#     n <- sys.call()
#     n <- as.character(n)[-1]
#     if (!is.null(n2 <- names(result))) {
#       which <- n2 != ""
#       n[which] <- n2[which]
#     }
#     names(result) <- n
#   }
#   result
# }
#
# Due to copy issue in the CRAN
# I am writing my own easy-version of the enlist function
enlist = function (...) {
  result = list(...)

  the_call = sys.call()
  names = as.character(the_call)[-1]
  names(result) = names
  return(result)
}



#' Compute the conditional number of design matrix
#'
#' @param x matrix to be used in svd
#' @export
#' @examples
#' data("pringles_fat")
#' data_fat = pringles_fat
#' h_tmp = 1.3
#' x = data_fat[,c("c1","c2","c3","x11","x12","x21","x22")]
#' find_condition_num (x)

find_condition_num = function (x) {
  singluar_values = base::svd(x)$d
  # This sensitivity of the solution x to changes in the right hand side b is a reflection of the condition number.
  # https://blogs.mathworks.com/cleve/2017/07/17/what-is-the-condition-number-of-a-matrix/
  cond_num = (max(singluar_values)/min(singluar_values))^2
  return (cond_num)
}

#' Check column correlations
#'
#' @param dat data.frame
#' @export
#' @examples
#' data("pringles_fat")
#' data_fat = pringles_fat
#' h_tmp = 1.3
#' x = data_fat[,c("c1","c2","c3","x11","x12","x21","x22")]
#' check_col_correlation (dat=x)
# the multiple scheffe model
# check column correlation
# https://stackoverflow.com/questions/22282531/how-to-compute-correlations-between-all-columns-in-r-and-detect-highly-correlate

check_col_correlation = function(dat){
  #library(dplyr); library(tidyr); library(tibble)
  value = var1 = var2 = NULL ## To please R CMD check
  dat_sub = dat
  d2 <- dat_sub %>%
    as.matrix %>%
    cor %>%
    as.data.frame %>%
    tibble::rownames_to_column(var = 'var1') %>%
    tidyr::gather(var2, value, -var1)
  d2
}


