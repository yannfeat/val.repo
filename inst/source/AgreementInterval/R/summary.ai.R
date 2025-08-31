
#' @title summary.ai
#' @description The summary method for ai objects
#' @author Jialin Xu, Jason Liao
#' @export
#' @rdname summary.ai
#' @param object ai object from ai function
#' @param ... additional arguments affecting the summary produced
#' @return Function summary.ai prints out key summaries on screen
#'
#' @examples
#' a <- c(1, 2, 3, 4, 7)
#' b <- c(1, 3, 2, 5, 3)
#' ans <- ai(x=a, y=b)
#' summary(ans)
#' @references Jason J. Z. Liao, Quantifying an Agreement Study, Int. J. Biostat. 2015; 11(1): 125-133

# This is the summary function to summarize and print out original data from "ai" object.
#
# Author: Jialin Xu, jxx120@gmail.com
#
# Reference: Jason J. Z. Liao, Quantifying an Agreement Study, Int. J. Biostat. 2015; 11(1): 125-133

summary.ai=function(object, ...){
#summary.ai=function(object, digits=3){
  digits=3
  x=object
  if (class(x)!="ai"){
    msg="Please input ai object running through improvedBA function"
    stop(msg)
  }
  result=x

  cat(" ------- Summary --------\n")
  cat(result$x.name, " and ", result$y.name, "agree with each other at discordance rate of", result$alpha, "with tolerance probability of", round(result$tolProb, digits=digits), "from sample size n=", result$n, ".\n")
  if (sum(is.na(result$clin.limit))==0){
    cat("Given clinical relevant limit,", result$x.name, " and ", result$y.name, "agree with each other at discordance rate of", round(result$alpha.cl, digits=digits), "with tolerance probability of", round(result$tolProb.cl, digits=digits), "from the same sample size n=", result$n, ".\n")
  }
  cat("\n")
  cat(" ------- Summary Statistics -------", "\n")
  print(round(result$summaryStat, digits=digits));
  cat("Sigma e = ", round(result$sigma.e, digits=digits), "\n")
  cat("\n------- Concordance Assessment Using Index Approaches -------", "\n")
  print(round(result$indexEst, digits=digits))
  cat("\n------- Concordance Assessment Using Interval Approaches -------", "\n")
  print(round(result$intervalEst, digits=digits))
  cat("\n------- Bias Estimates -------", "\n")
  print(round(result$biasEst, digits=digits))
  cat("\n------- End of summary -------\n\n")



}
