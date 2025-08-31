#' @title aiAdj
#' @description Function aiAdj calculates bias-adjusted average interval from ai object
#' @author Jialin Xu, Jason Liao
#' @export
#' @rdname aiAdj
#' @param object ai object from ai function
#' @param x A numeric value or a vector of numeric values to calculate bias-adjusted average interval for
#' @return bias-adjusted and total-adjusted average interval for each value in \code{x}
#'
#' @details Function aiAdj uses proportional bias per \code{x} unit, Liao's average interval,
#' Liao's average interval adjusted for fixed bias to calculate bias-adjusted and total-adjusted average interval.
#'
#' @examples
#' ans <- ai(x=IPIA$Tomography, y=IPIA$Urography)
#' aiAdj(object=ans, x=1)
#' aiAdj(object=ans, x=c(1, 2))
#' @references Jason J. Z. Liao, Quantifying an Agreement Study, Int. J. Biostat. 2015; 11(1): 125-133


aiAdj=function(object, x){
  if (class(object)!="ai"){
    msg="Please input ai object running through improvedBA function"
    stop(msg)
  }
  if (!is.numeric(x) | !is.vector(x))
    stop("Error: please enter a numeric vector of x for adjusted average interval estimation.")

  result=object

  cnames=c("x",
           paste("Liao.AI.Adj.Propo.", result$conf.level, c("LL", "UL"), sep=""),
           paste("Liao.AI.Adj.Total.", result$conf.level, c("LL", "UL"), sep="")
  )

  pred=data.frame(matrix(NA, length(x), 5))
  if (length(x)>1) {
    pred[, 1]=x
    pred[, 2]=result$intervalEst["Liao.AI", 1]           + result$biasEst["Propo.Bias.at.x=1", "Est"]*x
    pred[, 3]=result$intervalEst["Liao.AI", 2]           + result$biasEst["Propo.Bias.at.x=1", "Est"]*x
    pred[, 4]=result$intervalEst["Liao.AI.Adj.Fixed", 1] + result$biasEst["Propo.Bias.at.x=1", "Est"]*x
    pred[, 5]=result$intervalEst["Liao.AI.Adj.Fixed", 2] + result$biasEst["Propo.Bias.at.x=1", "Est"]*x
    colnames(pred)=cnames
  }
  if (length(x)==1) {
    pred[1]=x
    pred[2]=result$intervalEst["Liao.AI", 1]           + result$biasEst["Propo.Bias.at.x=1", "Est"]*x
    pred[3]=result$intervalEst["Liao.AI", 2]           + result$biasEst["Propo.Bias.at.x=1", "Est"]*x
    pred[4]=result$intervalEst["Liao.AI.Adj.Fixed", 1] + result$biasEst["Propo.Bias.at.x=1", "Est"]*x
    pred[5]=result$intervalEst["Liao.AI.Adj.Fixed", 2] + result$biasEst["Propo.Bias.at.x=1", "Est"]*x
    names(pred)=cnames
  }
  return(pred)
}
