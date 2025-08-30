#' @export
#' @import stats
eBellBX<-function(p, a, lambda)
{
    fn = function(x) {
        vBellBX(x, a, lambda)
    }
    ES = p
    for (i in 1:length(p)) {
        ES[i] = (1/p[i]) * integrate(fn, lower = 0, upper = p[i],
            stop.on.error = FALSE)$value
    }
    return(ES)
}
