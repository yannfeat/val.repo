#' @export
#' @import stats
eBellEE<-function(p, alpha, beta, lambda)
{
    fn = function(x) {
        vBellEE(x, alpha, beta, lambda)
    }
    ES = p
    for (i in 1:length(p)) {
        ES[i] = (1/p[i]) * integrate(fn, lower = 0, upper = p[i],
            stop.on.error = FALSE)$value
    }
    return(ES)
}
