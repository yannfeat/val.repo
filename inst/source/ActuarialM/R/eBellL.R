#' @export
#' @import stats
eBellL<-function(p, b, q, lambda)
{
    fn = function(x) {
        vBellL(x, b, q, lambda)
    }
    ES = p
    for (i in 1:length(p)) {
        ES[i] = (1/p[i]) * integrate(fn, lower = 0, upper = p[i],
            stop.on.error = FALSE)$value
    }
    return(ES)
}
