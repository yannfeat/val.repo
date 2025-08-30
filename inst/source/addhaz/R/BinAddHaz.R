BinAddHaz <- function (formula, data, subset, weights, na.action, model = TRUE,
                       contrasts = NULL, start, attrib = TRUE,
                       attrib.var, collapse.background = FALSE, attrib.disease = FALSE,
                       type.attrib = "abs", seed, bootstrap = FALSE, conf.level = 0.95,
                       nbootstrap, parallel = FALSE,
                       type.parallel = "snow", ncpus = 4,...){
  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "weights", "na.action", "offset"),
             names(mf), 0)
  mf <- mf[c(1, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1]] <- quote(stats::model.frame)
  mf <- eval.parent(mf)
  mt <- attr(mf, "terms")
  y <- model.response(mf, "numeric")

  w <- as.vector(model.weights(mf))
  if (!is.null(w) && !is.numeric(w))
    stop("'weights' must be a numeric vector")
  if(is.null(w)){
    w <- rep(1, nrow(data))
  }

  if(mean(w) != 1){
    w <- w/mean(w)}

  offset <- model.offset(mf)
  x <- model.matrix(mt, mf, contrasts)

  ### Initial values
  if (missing(start)){
    init.val <- glm(formula, family = poisson, data = data)
    start <- abs(init.val$coefficients)} else{
      start <- start
    }

  ### Functions
  BinAddHazLogLik <- function(start.val, data, y, x, wgt){

    beta <- start.val[1: length(start.val)]

    eta_i <- as.vector(x %*% beta)
    pi_i <- 1 - exp(-eta_i)

    dev.resid <- function(y, pi_i, wgt){
      2 * wgt * (y * log(ifelse(y == 0, 1, y/pi_i)) + (1 - y) *
                   log(ifelse(y == 1, 1, (1 - y)/(1 - pi_i))))}

    LL <- sum(dev.resid(y, pi_i, wgt))
    return(LL)}

  AttribBin <- function(bin.coef, attrib.coef, data, y,
                        x.mat, att.var., wgt, type.attrib,
                        collapse.background = FALSE, attrib.disease = TRUE,
                        boots = FALSE){
    attrib.length <- length(unique(att.var.))
    haz.dis <- t(bin.coef * t(x.mat))
    if('(Intercept)' %in% names(attrib.coef)){
      haz.back <- rep(attrib.coef, nrow(x.mat))
    } else {
    haz.back <- attrib.coef[att.var.]}
    eta_i <- haz.back + apply(haz.dis, 1, sum)
    pi_i <- 1 - exp(-eta_i)
    att.x.mat <- (haz.dis/eta_i) * pi_i
    att.back <- (haz.back/eta_i) * pi_i
    att.mat <- matrix(NA, nrow = (ncol(x.mat) + 3), ncol = nlevels(att.var.))
    att.mat[1, ] <- tapply(wgt, att.var., sum)
    att.mat[2, ] <- tapply(pi_i * wgt, att.var., sum)
    att.mat[3, ] <- tapply(att.back * wgt, att.var., sum)

    for (i in 1:ncol(x.mat)) {
      att.mat[3 + i, ] <- tapply(att.x.mat[, i] * wgt,
                                 att.var., sum)
    }
    dimnames(att.mat) <- list(c("nn", "disab", "backgrnd",
                                colnames(x.mat)), paste0("att.var", levels(att.var.)))

    att.mat1 <- as.matrix(apply(att.mat, 1, sum), ncol = 1)

    attribution <- list()
    attribution2 <- list()
    att.rel <- list()
    att.abs <- list()
    att.final <- list()

    if(collapse.background == FALSE){
      for (i in 1:ncol(att.mat)) {
        attribution[[i]] <- att.mat[, i]
        row_sub <- attribution[[i]] != 0
        attribution2[[i]] <- attribution[[i]][row_sub]

        att.rel[[i]] <- attribution2[[i]][3:length(attribution2[[i]])]/attribution2[[i]][2]
        att.abs[[i]] <- attribution2[[i]][2:length(attribution2[[i]])]/attribution2[[i]][1]
    }

      att.final.rel <- matrix(unlist(att.rel), ncol = 1,
                        dimnames = list(paste0(names(unlist(att.rel)), ".",
                                               rep(levels(att.var.), each = length(att.rel[[1]]))),
                                        paste0("att.rel")))
      att.final.abs <- matrix(unlist(att.abs), ncol = 1,
                        dimnames = list(paste0(names(unlist(att.abs)), ".",
                                               rep(levels(att.var.), each = length(att.abs[[1]]))),
                                        paste0("att.abs")))

      if (type.attrib == "rel") {
        Output <- att.final.rel
      }
      if (type.attrib == "abs") {
        Output <- att.final.abs
      }
      if (type.attrib == "both") {
        Output <- list(att.rel = att.final.rel, att.abs = att.final.abs)}

    } else { #collapse.background == TRUE
      for (i in 1:ncol(att.mat1)) {
        attribution[[i]] <- att.mat1[, i]
        row_sub <- attribution[[i]] != 0
        attribution2[[i]] <- attribution[[i]][row_sub]

        if (type.attrib == "rel") {
          att.final[[i]] <- attribution2[[i]][3:length(attribution2[[i]])]/attribution2[[i]][2]
        }
        if (type.attrib == "abs") {
          att.final[[i]] <- attribution2[[i]][2:length(attribution2[[i]])]/attribution2[[i]][1]
        }
        if (type.attrib == "both") {
          att.rel[[i]] <- attribution2[[i]][3:length(attribution2[[i]])]/attribution2[[i]][2]
          att.abs[[i]] <- attribution2[[i]][2:length(attribution2[[i]])]/attribution2[[i]][1]
          att.final <- list(att.rel = att.rel, att.abs = att.abs)
        }
      }

      if (type.attrib == "rel" | type.attrib == "abs") {
        Output <- matrix(unlist(att.final), ncol = 1,
                         dimnames = list(names(unlist(att.final)),
                                         paste0("att.", type.attrib)))
      }

      if (type.attrib == "both") {
        if(boots == FALSE){
          Out <- matrix(NA, ncol = length(att.final),
                        nrow = length(att.final$att.abs) * length(att.final$att.abs[[1]]),
                        dimnames = list(names(unlist(att.final[[2]])), names(att.final)))

          Out[,"att.abs"] <- unlist(att.final$att.abs)
          indexOut <- (1:(nrow(Out)/length(att.final$att.abs[[1]]))-1)*length(att.final$att.abs[[1]])+1
          Out[-indexOut,"att.rel"] <- unlist(att.final$att.rel)
          Output <- Out
        } else {
          Output <- list(att.rel = att.final$att.rel, att.abs = att.final$att.abs)
        }

      }
    }
    return(Output)
  }

  AttribBinSimple <- function(Coeff = Coeff, data, x, y, wgt, type.attrib,
                              att.var. = att.var., collapse.background = FALSE){
    haz <- t(Coeff * t(x))
    eta_i <- apply(haz, 1, sum)
    pi_i <- 1 - exp(-eta_i)
    att. <- (haz/eta_i) * pi_i

    att.mat <- matrix(NA, nrow = (ncol(x) + 2), ncol = 1)
    att.mat[1,] <-  sum(wgt)
    att.mat[2,] <-  sum(pi_i * wgt)
    for (i in 1: ncol(x)){
      att.mat[2 + i,] <- sum(att.[, i] * wgt)
    }
    dimnames(att.mat) <- list(c("nn","disab", colnames(x)))

    if (collapse.background == TRUE & (!missing(att.var.))){
      interval <- 2 + length(unique(att.var.))
      att.mat2 <- matrix(c(att.mat[1:2,],
                           background = sum(att.mat[3:interval,]),
                           att.mat[(interval + 1) : nrow(att.mat),]),
                         nrow = ncol(x) - length(unique(att.var.)) + 3, ncol = 1)
      dimnames(att.mat2) <- list(c("nn","disab", "background", colnames(x[,-c(1:length(unique(att.var.)))])))
      att.mat <- att.mat2
    }

    attribution <- matrix(NA, ncol = ncol(att.mat), nrow = nrow(att.mat),
                          dimnames = list(rownames(att.mat)))

    for(i in 1: ncol(att.mat)){
      row_sub <- att.mat[, i] != 0
      attribution[, i] <- att.mat[, i][row_sub]
      att.rel <- attribution[2: nrow(attribution), i]/attribution[2,i]
      att.abs <- attribution[2: nrow(attribution), i]/attribution[1,i]
    }

    att.final.rel <- matrix(att.rel[-1], ncol = 1, dimnames = list(names(att.rel[-1]),
                                                                   "att.rel"))
    att.final.abs <- matrix(att.abs, ncol = 1, dimnames = list(names(att.abs),
                                                               "att.abs"))

    if(type.attrib == "rel"){
      att.final <- att.final.rel
    }

    if(type.attrib == "abs"){
      att.final <- att.final.abs
    }

    if(type.attrib == "both"){
      att.final <- list(att.rel = att.final.rel, att.abs = att.final.abs)
    }
    return(att.final)
  }

  CovAddHaz <- function(start, y, x){
    y.vec <- y
    x <- x
    beta <- start
    eta_i <- as.vector(x %*% beta)
    pi_i <- 1 - exp(-eta_i)
    W <- diag(drop((1 - pi_i)/pi_i))
    V <- ginv(t(x) %*% W %*% x)
    return(V)}

  ### Optimization
  BinLL <- constrOptim(theta = start, data = data, f = BinAddHazLogLik, ui = x,
                       ci = rep(0, nrow(x)), method = "Nelder-Mead",  y = y, x = x,
                       wgt = w)
  Coeff <- BinLL$par

  ### Attribution
  if(attrib){
    if (missing(attrib.var)){
    if (collapse.background == FALSE) {
      Attribution <- AttribBinSimple(Coeff = Coeff, data = data, x = x, y = y, wgt = w,
                                     type.attrib = type.attrib, collapse.background = FALSE)
    }
    if (collapse.background == TRUE | attrib.disease == TRUE){
      stop("Cannot calculate contribution; attrib.var is missing")
      }
      } else {
        att.var <- data[, deparse(substitute(attrib.var))]
        if ('(Intercept)' %in% names(Coeff)) {
          x.mat <- x[, -1]
        } else {
        x.mat <- x[, -c(1:length(unique(att.var)))]}
        att.var.levels <- sort(unique(att.var))
        attrib.length <- length(unique(att.var))
        if (!is.factor(att.var)) {
          att.var. <- factor(att.var, levels = att.var.levels,
                             labels = att.var.levels)
        }
        if (is.factor(att.var)) {
          att.var. <- att.var
        }
        if (attrib.length == 1 | '(Intercept)' %in% names(Coeff)) {
          attrib.coef = Coeff[1]
          bin.coef = Coeff[-1]
        } else {
          attrib.coef = Coeff[1:attrib.length]
          bin.coef = Coeff[-c(1:attrib.length)]
        }

        if (attrib.disease == FALSE){
        if (collapse.background == FALSE){
          Attribution <- AttribBinSimple(Coeff = Coeff, data = data, x = x, y = y, wgt = w,
                                         type.attrib = type.attrib, collapse.background = FALSE,
                                         att.var. = att.var.)
        } else {
          Attribution <- AttribBinSimple(Coeff = Coeff, data = data, x = x, y = y, wgt = w,
                                         type.attrib = type.attrib, collapse.background = TRUE,
                                         att.var. = att.var.)

        }
      }

        if(attrib.disease == TRUE){
          if (collapse.background == FALSE){
          Attribution <- AttribBin(bin.coef = bin.coef, attrib.coef = attrib.coef, data = data, y = y,
                                   x.mat = x.mat, att.var. = att.var., wgt = w, type.attrib = type.attrib,
                                   collapse.background = FALSE)
        }
          if (collapse.background == TRUE){
          Attribution <- AttribBin(bin.coef = bin.coef, attrib.coef = attrib.coef, data = data, y = y,
                                   x.mat = x.mat, att.var. = att.var., wgt = w, type.attrib = type.attrib,
                                   collapse.background = TRUE)
        }
      }
    }
  }


  ### CI
  if(bootstrap == FALSE){
    vcov <- CovAddHaz(start = BinLL$par, x = x, y = y)
    stdError <- sqrt(diag(vcov))

    CILow <- BinLL$par - (1.96 * stdError)
    CIHigh <- BinLL$par + (1.96 * stdError)
    pvalue <- round(2 * pnorm(-abs(BinLL$par/stdError)), 4)
    CI <- cbind(CILow, CIHigh)
    colnames(CI) <- c("CI2.5", "CI97.5")
    colnames(vcov) <- names(BinLL$par)
    rownames(vcov) <- names(BinLL$par)

    if(attrib){
      Results <- list(coefficients = BinLL$par, ci = CI, resDeviance = BinLL$value,
                      df = nrow(data) - length(BinLL$par), pvalue = pvalue,
                      stdError = stdError, vcov = vcov, contribution = Attribution)
    } else {
      Results <- list(coefficients = BinLL$par, ci = CI, resDeviance = BinLL$value,
                      df = nrow(data) - length(BinLL$par), pvalue = pvalue,
                      stdError = stdError, vcov = vcov, contribution = attrib)}
  }

  ### bootstrap CI
  if(bootstrap){
    if(attrib){
      if(attrib.disease){
      mystat <- function(data, indices) {
        m1 <-  constrOptim(theta = BinLL$par, data = data[indices,], f = BinAddHazLogLik,
                           ui = x[indices,], ci = 0, method = "Nelder-Mead", y = y[indices],
                           x = x[indices,], wgt = w[indices])

        m2 <- AttribBin(bin.coef = bin.coef, attrib.coef = attrib.coef,
                        data = data[indices,], y = y[indices], x.mat = x.mat[indices,],
                        att.var. = att.var.[indices], wgt = w[indices],
                        type.attrib = type.attrib, collapse.background = collapse.background,
                        attrib.disease = TRUE, boots = TRUE)

        if(is.list(m2)){
          m3 <- unlist(m2)} else {
            m3 <- m2}
        res <- c(m1$par, m3)
        return(res)
      }
    }
      if(attrib.disease == FALSE){
        mystat <- function(data, indices) {
          m1 <-  constrOptim(theta = BinLL$par, data = data[indices,], f = BinAddHazLogLik,
                             ui = x[indices,], ci = 0, method = "Nelder-Mead", y = y[indices],
                             x = x[indices,], wgt = w[indices])

          m2 <- AttribBinSimple(Coeff = Coeff, data = data[indices,], x = x[indices,],
                                y = y[indices], wgt = w[indices], type.attrib = type.attrib,
                                att.var. = att.var.[indices],
                                collapse.background = collapse.background)


          if(is.list(m2)){
            m3 <- unlist(m2)} else {
              m3 <- m2}
          res <- c(m1$par, m3)
          return(res)
    }
      }
  } else {
      mystat <- function(data, indices) {
        m <-  constrOptim(theta = BinLL$par, data = data[indices,], f = BinAddHazLogLik,
                          ui = x[indices,], ci = 0, method = "Nelder-Mead", y = y[indices],
                          x = x[indices,], wgt = w[indices])
        return(m$par)
      }
    }

    if(!missing(seed)){
      set.seed(seed)}

    if (parallel == TRUE){
      BootResult <- boot(data = data, statistic = mystat, R = nbootstrap, parallel = type.parallel,
                         ncpus = ncpus)} else {
                           BootResult <- boot(data = data, statistic = mystat, R = nbootstrap)
                         }

    BootCI <- matrix(NA, ncol = 2, nrow = length(BootResult$t0))

    for (i in 1:length(BootResult$t0)){
      BootCI[i,] <- boot.ci(BootResult,  conf = conf.level, type = "perc", index = i)[[4]][, 4:5]}
    colnames(BootCI) <- c("CILow", "CIHigh")

    if(attrib){
      CILow.att <- matrix(BootCI[(length(start) + 1): length(BootResult$t0), 1], ncol = 1,
                          dimnames = list(names(BootResult$t0)[(length(start) + 1): length(BootResult$t0)],
                                          "CILow"))
      CIHigh.att <- matrix(BootCI[(length(start) + 1): length(BootResult$t0), 2], ncol = 1,
                           dimnames = list(names(BootResult$t0)[(length(start) + 1): length(BootResult$t0)],
                                           "CIHigh"))

      if(type.attrib == "both"){
        att1 <- list(att.rel = cbind(Attribution$att.rel, CILow.att[1: (nrow(Attribution$att.rel)),],
                                     CIHigh.att[1: (nrow(Attribution$att.rel)),]),
                     att.abs = cbind(Attribution$att.abs,
                                     CILow.att[(nrow(Attribution$att.rel)+1) :nrow(CILow.att),],
                                     CIHigh.att[(nrow(Attribution$att.rel)+1) :nrow(CILow.att),]))
        colnames(att1[[1]]) <- c("Contribution", "CILow", "CIHigh")
        colnames(att1[[2]]) <- c("Contribution", "CILow", "CIHigh")
      } else {
        att1 <- cbind(Attribution, CILow.att, CIHigh.att)
      }
      Results <- list(coefficients = Coeff, ci = cbind(matrix(BootCI[1: length(start),
                                                                     "CILow"], ncol = 1),
                                                       matrix(BootCI[1:length(start),
                                                                     "CIHigh"], ncol = 1)),
                      resDeviance = BinLL$value, df = nrow(data) - length(BinLL$par),
                      contribution = att1, bootsRep = BootResult$t, conf.level = conf.level)

      if(class(Attribution) == "list"){
        colnames(Results$bootsRep) <- c(names(Coeff), paste0("att.rel.", rownames(Attribution$att.rel)),
                                        paste0("att.abs.", rownames(Attribution$att.abs)))
      } else {
      colnames(Results$bootsRep) <-  c(names(Coeff), paste0("att.", rownames(Attribution)))
      }
      colnames(Results$ci) <- c("CILow", "CIHigh")
      rownames(Results$ci) <- names(Coeff)
    } else {
      Results <- list(coefficients = Coeff, ci = cbind(matrix(BootCI[1: length(start),
                                                                     "CILow"], ncol = 1),
                                                       matrix(BootCI[1:length(start),
                                                                     "CIHigh"], ncol = 1)),
                      resDeviance = BinLL$value, df = nrow(data) - length(BinLL$par),
                      contribution = attrib, bootsRep = BootResult$t, conf.level = conf.level)
      colnames(Results$bootsRep) <- names(BootResult$t0)
      colnames(Results$ci) <- c("CILow", "CIHigh")
      rownames(Results$ci) <- names(Coeff)
    }
  }
  Results$bootstrap <- bootstrap
  Results$fitted.values <- as.vector(x %*% Results$coefficients)
  Results$residuals <- y - Results$fitted.values
  Results$call <- match.call()
  class(Results) <- "binaddhazmod"
  return(Results)
}

# GENERIC FUNCTIONS IN S3 LANGUAGE (PRINT, SUMMARY, PREDICT)
binaddhazmod <- function(x, ...) UseMethod("binaddhazmod")

print.binaddhazmod <- function(x,...){
  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(x$coefficients)
}

summary.binaddhazmod <- function(object, ...){
  if(object$bootstrap == FALSE){
    se <- sqrt(diag(object$vcov))
    tval <- coef(object)/se
    TAB <- cbind(Estimate = coef(object),
                 StdErr = se,
                 t.value = tval,
                 p.value  = 2*pt(-abs(tval), df=object$df))} else {
                   TAB <- cbind(Estimate = coef(object), CI = object$ci)
                 }
  if(object$bootstrap == FALSE){
    res <- list(call = object$call, bootstrap = object$bootstrap,
                coefficients = TAB)} else {
                  res <- list(call = object$call, bootstrap = object$bootstrap,
                              coefficients = TAB, conf.level = object$conf.level)}
  class(res) <- "summary.binaddhazmod"
  res
}

predict.binaddhazmod <- function(object, newdata = NULL, ...){
  if(is.null(newdata))
    y <- fitted(object)
  else{
    if(!is.null(object$formula)){
      x <- model.matrix(object$formula, newdata)
    }
    else{
      x <- newdata
    }
    y <- as.vector(x %*% coef(object))
  }
  y
}

