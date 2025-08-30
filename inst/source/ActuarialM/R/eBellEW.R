#' @export
#' @import stats
eBellEW<-function(p, alpha, beta, theta,lambda)
{
    fn = function(x) {
        vBellEW(x, alpha, beta, theta, lambda)
    }
    ES = p
    for (i in 1:length(p)) {
        ES[i] = (1/p[i]) * integrate(fn, lower = 0, upper = p[i],
            stop.on.error = FALSE)$value
    }
    return(ES)
}
