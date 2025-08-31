#' @title tolProb
#' @description Function tolProb calculates tolerance probability based on
#' sample size (n), number of discordance pairs (k) and discordance rate (alpha).
#' @author Jialin Xu, Jason Liao
#' @export
#' @rdname tolProb
#' @param n Sample size
#' @param k Number of discordance pairs, discordance pairs are defined as samples with difference greater than average interval
#' @param alpha Discordance rate, default 0.05.
#' @return tolerance probability
#'
#' @details Function tolProb calculates tolearance probability based on sample size(n), number of discordance pairs (k) and discordance rate (alpha).
#' Its value is calculated as the largest value such that the following inequality is true:
#'  \deqn{1-\sum _{i=0}^{k} {n\choose i} * {(1-\alpha)}^{n-i} * {\alpha}^i \ge \beta}
#'
#'
#' @examples
#' tolProb(n=52, k=5, alpha=0.05)
#' tolProb(n=52, k=0, alpha=0.05)
#' @references Jason J. Z. Liao, Quantifying an Agreement Study, Int. J. Biostat. 2015; 11(1): 125-133

## Function to calculate tolerance probability based on sample size (n) and number of paired data out of limit (k)

tolProb=function(n, k, alpha=0.05){
  if (length(n)!=1)
    stop("Please enter n as one single integer value")
  if (length(k)!=1)
    stop("Please enter k as one single integer value")
  if (!is.numeric(n) | length(n)!=1 |
      !is.numeric(k) | length(k)!=1 | n<k)
    stop("Please enter n and k as integer and make sure n>=k")
  if (length(alpha)!=1 | !is.numeric(alpha))
    stop("Please enter a numeric value of alpha between 0 and 1, default 0.05")
 i=0:k
 x1=choose(n, i)
 x2=(1-alpha)^(n-i)
 x3=alpha^i
 x0=sum(x1*x2*x3)
 beta=1-x0
 return(beta)
}


