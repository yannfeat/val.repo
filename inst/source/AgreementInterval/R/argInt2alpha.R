#' @title agrInt2alpha
#' @description Function agrInt2alpha calculates discordance rate (alpha) using clinically meaningful limit.
#' @author Jialin Xu, Jason Liao
#' @export
#' @rdname agrInt2alpha
#' @param clin.limit Clinically meaningful lower and upper limit
#' @param n Sample size
#' @param sigmae Variance estimate of residual from measurement error model
#' @return Discordance rate
#'
#' @details Function agrInt2alpha calculates discordance rate (alpha) using clinically meaningful limit.
#'
#' @examples
#' agrInt2alpha(clin.limit=c(-15, 15), n=52, sigmae=46.09245)
#' @references Jason J. Z. Liao, Quantifying an Agreement Study, Int. J. Biostat. 2015; 11(1): 125-133


agrInt2alpha=function(clin.limit, n, sigmae){
  if (length(clin.limit)!=2 | !is.numeric(clin.limit))
    stop("Error: clin.limit has to be numeric vector of length 2")
  if (!is.numeric(n) | n<0 | length(n)!=1 | !is.numeric(sigmae) | sigmae<0 | length(sigmae)!=1 )
    stop("Error: n and sigmae should be numeric values greater than 0, of length 1")
  width=diff(clin.limit)
  qt=width/2/sqrt(2*sigmae)
  result=2*(1-pt(qt, n-1))
  return(result)
}
