ices <- function(formula, data, model = TRUE, x = FALSE, y = FALSE, qr = TRUE) {
    ret.x <- x
    ret.y <- y
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval(mf, parent.frame())
    mt <- attr(mf, "terms")
    y <- model.response(mf, "numeric")
    x <- model.matrix(mt, mf)
    z <- QRS(x, y, Nsims = 100)
    z$call <- cl
    z$terms <- mt
    z$names <- names(mf)
    if (model)
        z$model <- mf
    if (ret.x)
        z$x <- x
    if (ret.y)
        z$y <- y
    if (!qr)
        z$qr <- NULL
    class(z)<- c("QRS", "lm")
    z
}

