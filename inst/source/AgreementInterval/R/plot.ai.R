
#' @title plot.ai
#' @description The plot method for ai objects
#' @author Jialin Xu, Jason Liao
#' @export
#' @rdname plot.ai
#' @param x ai object from ai function
#' @param clin.limit Clinically meaningful lower and upper limit
#' @param which Index parameter to control which plot to output, by default, all four plots will be outputed.
#' @param ... Additional arguments to be passed to the round function and to control number of decimals in the display.
#' @return Function plot.ai returns 2 by 2 plots (See details)
#'
#' @details The four plots include 1) scatterplot of raw data with regression line from the measurement error model,
#' 2) Difference between two measurement methods with original average interval determined by alpha and clinically meaningful lower and upper limit,
#' 3) Difference between two measurement methods  with average interval adjusted for fixed bias,
#' as well as 4) Sorted difference bewteen two measurement methods with average interval adjusted for total bias.
#'
#' @examples
#' a <- c(1, 2, 3, 4, 7)
#' b <- c(1, 3, 2, 5, 3)
#' ans <- ai(x=a, y=b)
#' plot(x=ans)
#' plot(x=ans, clin.limit=c(-5, 5))
#' @importFrom graphics abline legend lines par plot
#' @references Jason J. Z. Liao, Quantifying an Agreement Study, Int. J. Biostat. 2015; 11(1): 125-133

# This is the plot function to plot original data and average interval limit from "ai" object.
#
# Author: Jialin Xu, Jason Liao
#
# Reference: Jason J. Z. Liao, Quantifying an Agreement Study, Int. J. Biostat. 2015; 11(1): 125-133

plot.ai=function(x, clin.limit=NA, which=1:4, ...){
  if (class(x)!="ai"){
    msg="Please input ai object running through improvedBA function"
    stop(msg)
  }
  result=x

  range.y=
    range(
      result$intervalEst["Liao.AI", 1], clin.limit, result$y-result$x,
      result$intervalEst["Liao.AI.Adj.Fixed", 1],result$y-result$x, clin.limit-result$intercept,
      result$intercept+(result$slope-1)*result$x[order(result$x)]+result$intervalEst["Liao.AI.Adj.Fixed", 1],
      result$intercept+(result$slope-1)*result$x[order(result$x)]+result$intervalEst["Liao.AI.Adj.Fixed", 2],
      na.rm=TRUE
    )

  ######## NEED TO REVISE based on current output object
  if (sum(!is.na(clin.limit))>0 | sum(!is.na(x$clin.limit))>0){
    if (sum(!is.na(clin.limit))==0)
        clin.limit=x$clin.limit
    leg.tex2=paste("Clin. limit: (", round(clin.limit[1], ...), ", ", round(clin.limit[2], ...), ")", sep="")
    leg.tex2b=paste("Clin. limit: (", round(clin.limit[1]-result$intercept, ...), ", ", round(clin.limit[2]-result$intercept, ...), ")", sep="")
    lty2=2
  }

  if (length(which)>4 | sum(which>4)>0) {
    stop("Please specify plot numbers between 1 and 4")
  }

  if (length(which)==4) pframe=c(2, 2)
  if (length(which)==3) pframe=c(1, 3)
  if (length(which)==2) pframe=c(1, 2)
  if (length(which)==1) pframe=c(1, 1)

  x.name=result$x.name
  y.name=result$y.name

  par(mfrow=pframe)

  if (sum(which==1)>0){
    plot.main="Raw"
    plot(result$x,result$y, xlab=x.name, ylab=y.name, main=plot.main)
    abline(0,1,lty=1)
    abline(result$intercept,result$slope,lty=2)
    legend(x="bottomright", legend=c("U=T", "model"), lty=c(1, 2), bty="n")
  }

  if (sum(which==2)>0){
    plot.main="Agreement Interval"
    plot(1:result$summaryStat["x", "n"],result$y-result$x,
          xlab="Samples",ylab=paste("Diff. (", y.name,  " - ", x.name, ")", sep=""),pch=1,main=plot.main,
          ylim=range.y)
    abline(h=0,lty=3)
    abline(h=result$intervalEst["Liao.AI", ],lty=1) # doscordance rate alpha=0.05
    leg.tex1=paste("(", round(result$intervalEst["Liao.AI", 1], ...), ", ", round(result$intervalEst["Liao.AI", 2], ...), ")", sep="")
    lty1=1
    if (sum(!is.na(clin.limit))>0){
      abline(h=clin.limit, lty=2) # clinical meaningful limit
      legend(x="bottomleft", legend=c(leg.tex1, leg.tex2), lty=c(lty1, lty2), bty="n")
    } else {
      legend(x="bottomleft", legend=leg.tex1, lty=lty1, bty="n")
    }

  }

  if (sum(which==3)>0){
    plot.main="Agreement Interval Adjusted for Fixed Bias"
    leg.tex1=paste("(", round(result$intervalEst["Liao.AI.Adj.Fixed", 1], ...), ", ", round(result$intervalEst["Liao.AI.Adj.Fixed", 2], ...), ")", sep="")
    lty1=1
    leg.tex=ifelse(is.na(clin.limit), leg.tex1, c(leg.tex1, leg.tex2b))
    lty=ifelse(is.na(clin.limit), lty1, c(lty1, lty2))


    plot(1:result$summaryStat["x", "n"],result$y-result$x,
        xlab="Samples",ylab=paste("Diff. (", y.name,  " - ", x.name, ")", sep=""),pch=1,main=plot.main,
        ylim=range.y)
    abline(h=result$intercept, lty=3)
    abline(h=result$intervalEst["Liao.AI.Adj.Fixed", ], lty=1)
    if (sum(!is.na(clin.limit))>0) {
      abline(h=clin.limit-result$intercept, lty=2)
    }
    legend(x="bottomleft", legend=leg.tex, lty=lty, bty="n")
  }

  if (sum(which==4)>0){
    plot.main="Difference vs. sorted samples"
    plot(1:result$summaryStat["x", "n"],(result$y-result$x)[(1:result$summaryStat["x", "n"])[order(result$x)]],
       xlab="Sorted Samples",ylab=paste("Diff. (", y.name,  " - ", x.name, ")", sep=""),pch=1,main=plot.main,
       ylim=range.y)
    lines(1:result$n,result$intercept+(result$slope-1)*result$x[order(result$x)],lty=2)
    lines(1:result$n,result$intercept+(result$slope-1)*result$x[order(result$x)]+result$intervalEst["Liao.AI.Adj.Fixed", 1],lty=1)
    lines(1:result$n,result$intercept+(result$slope-1)*result$x[order(result$x)]+result$intervalEst["Liao.AI.Adj.Fixed", 2],lty=1)
  }
}
