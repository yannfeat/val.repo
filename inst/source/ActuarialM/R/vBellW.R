#' @export
#' @import stats
vBellW<-function (p, alpha , beta, lambda, log.p = FALSE, lower.tail = TRUE)
{
    if (log.p == TRUE)
        p = exp(p)
    if (lower.tail == FALSE)
        p = 1 - p
		t=-1/lambda*log(1-((log(1-p[p >= 0 & p <= 1]*(1-(exp(-exp(lambda)+1)))))/(-exp(lambda))))
    VaR=(-1/alpha*(log(1-(t))))^1/beta
    return(VaR)
}



