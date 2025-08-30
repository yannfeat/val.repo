MultAddHaz <- function(formula, data, subset, weights, na.action,
                       model = TRUE, contrasts = NULL, start,
                       attrib = TRUE, attrib.var,
                       collapse.background = FALSE, attrib.disease = FALSE,
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

  ### Factor y
  y.levels <- sort(unique(y))

  if(is.factor(y)){
    y. <- model.matrix(~y - 1)
  }

  if(!is.factor(y)){
    y.. <- factor(y, levels = y.levels, labels = y.levels)
    y. <- model.matrix(~y.. -1)
  }
  colnames(y.) <- y.levels
  y.resp <- y.[,-1]

  ### Starting values
  if(!missing(seed)){
    set.seed(seed)}

  if(missing(start)){
    start <- rep(sample(seq(0.1,0.9, by = 0.1), (length(unique(y)) -1)),
                 each = ncol(x))
  }
  ### Functions
  MultAddHazLogLik <- function(start, data, y, x, wgt){
    beta_ij <- matrix(unlist(split(start, cut(seq_along(start), ncol(y.resp),
                                              labels = FALSE))), ncol = ncol(y.resp))
    eta_ij <- matrix(NA, ncol = ncol(beta_ij), nrow = nrow(x))
    pi_ij <- matrix(NA, ncol = ncol(beta_ij), nrow = nrow(x))

    for (i in 1:ncol(beta_ij)){
      eta_ij[,i] <- as.vector(x %*% beta_ij[,i])}

    pi_i = 1-exp(-(apply(eta_ij, 1, sum)))
    for (i in 1:ncol(beta_ij)){
    pi_ij[,i] <- pi_i *(eta_ij[,i]/apply(eta_ij, 1, sum))}

    sum.y <- apply(y.resp, 1, sum)
    sum.pi_ij <- apply(pi_ij, 1, sum)

     LL <- -sum(2 * wgt * (((1 - sum.y) * log(ifelse(sum.y == 1, 1, (1 - sum.pi_ij)))) +
                          apply((y.resp * log(ifelse(y.resp == 0, 1, (pi_ij)))),
                                1, sum)))
     return(LL)}

  AttribMult <- function(mult.coef, attrib.coef, data, y, y.resp, x.mat, att.var. = NULL,
                         wgt, type.attrib,
                         collapse.background = FALSE, attrib.disease = FALSE){

    id.disab <- list()
    hdis <- list()
    haz.back <- list()
    eta_ij <- list()
    pi_ij <- list()
    att.x <- list()
    att.back <- list()
    att.mat <- list()
    att.mat3 <- list()

    if(is.null(att.var.)){
      for (i in 1:ncol(y.resp)){
        id.disab[[i]] <- which(y == 0 | y == i)
        hdis[[i]] <- t(mult.coef[, i] * t(x.mat))
        eta_ij[[i]] <- apply(hdis[[i]], 1, sum)
        pi_i = 1-exp(-(Reduce("+",eta_ij)))
        sum.eta_ij <- Reduce("+",eta_ij)
        pi_ij[[i]] <- eta_ij[[i]]/sum.eta_ij * pi_i

        att.x[[i]] <- (hdis[[i]]/eta_ij[[i]]) * pi_ij[[i]]
      }

      if(collapse.background == FALSE){
        for (i in 1:ncol(y.resp)){
          att.mat[[i]] <- matrix(NA, nrow = (ncol(x.mat) + 2), ncol = 1)
          att.mat[[i]][1,] <-  sum(wgt[id.disab[[i]]])
          att.mat[[i]][2,] <-  sum(pi_ij[[i]][id.disab[[i]]] * wgt[id.disab[[i]]])

          for (j in 1: ncol(x.mat)){
            att.mat[[i]][2 + j,] <- sum(att.x[[i]][, j][id.disab[[i]]] * wgt[id.disab[[i]]])
          }}

        att.mat2 <- matrix(unlist(att.mat), ncol = length(att.mat),
                           dimnames = list(c("nn","disab", colnames(x.mat)),
                                           paste0("y", colnames(y.resp))))

        attribution <- matrix(NA, ncol = ncol(att.mat2), nrow = nrow(att.mat2),
                              dimnames = list(rownames(att.mat2), colnames(att.mat2)))
        att.rel <- matrix(NA, ncol = ncol(att.mat2), nrow = nrow(att.mat2) -1,
                          dimnames = list(rownames(att.mat2[-1,]),
                                          paste0("att.rel.", colnames(att.mat2))))
        att.abs <- matrix(NA, ncol = ncol(att.mat2), nrow = nrow(att.mat2) -1,
                          dimnames = list(rownames(att.mat2[-1,]),
                                          paste0("att.abs.", colnames(att.mat2))))

        for(i in 1: ncol(attribution)){
          row_sub <- att.mat2[, i] != 0
          attribution[, i] <- att.mat2[, i][row_sub]
          att.rel[,i] <- attribution[2: nrow(attribution), i]/attribution[2,i]
          att.abs[,i] <- attribution[2: nrow(attribution), i]/attribution[1,i]
        }

        att.final.rel <- matrix(att.rel[-1,], ncol = 1,
                                dimnames = list(paste0(rep(rownames(att.rel[-1,]), ncol(att.rel)),
                                                       ".", rep(colnames(att.mat2), each = nrow(att.rel[-1,]))),
                                                "att.rel"))
        att.final.abs <- matrix(att.abs, ncol = 1, dimnames = list(paste0(rep(rownames(att.abs),
                                                                              ncol(att.abs)), ".", rep(colnames(att.mat2),
                                                                                                       each = nrow(att.abs))),
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
      }
      if (collapse.background == TRUE | attrib.disease == TRUE){
        stop("Cannot calculate contribution; attrib.var is missing")
      }
    } else { # if att.var.not missing

      for (i in 1:ncol(y.resp)){
        id.disab[[i]] <- which(y == 0 | y == i)
        hdis[[i]] <- t(mult.coef[, i] * t(x.mat))
        haz.back[[i]] <- attrib.coef[,i][att.var.]
        eta_ij[[i]] <- haz.back[[i]] + apply(hdis[[i]], 1, sum)
        pi_i = 1-exp(-(Reduce("+",eta_ij)))
        sum.eta_ij <- Reduce("+",eta_ij)
        pi_ij[[i]] <- eta_ij[[i]]/sum.eta_ij * pi_i

        att.x[[i]] <- (hdis[[i]]/eta_ij[[i]]) * pi_ij[[i]]
        att.back[[i]] <- (haz.back[[i]]/eta_ij[[i]]) * pi_ij[[i]]
      }

      if(attrib.disease == FALSE & collapse.background == TRUE){
        att.mat <- matrix(NA, nrow = (ncol(x.mat) + 3), ncol = ncol(y.resp))
        for (i in 1:ncol(y.resp)){
          att.mat[1, i] <-  sum(wgt[id.disab[[i]]])
          att.mat[2, i] <-  sum(pi_ij[[i]][id.disab[[i]]] * wgt[id.disab[[i]]])
          att.mat[3, i] <-  sum(att.back[[i]][id.disab[[i]]] * wgt[id.disab[[i]]])

          for (j in 1: ncol(x.mat)){
            att.mat[3 + j, i] <- sum(att.x[[i]][, j][id.disab[[i]]] * wgt[id.disab[[i]]])
          }
        }
        dimnames(att.mat) <- list(c("nn","disab","backgrnd", colnames(x.mat)),
                                  paste0("y", colnames(y.resp)))

        attribution <- matrix(NA, ncol = ncol(att.mat), nrow = nrow(att.mat),
                              dimnames = list(rownames(att.mat), colnames(att.mat)))
        att.rel <- matrix(NA, ncol = ncol(att.mat), nrow = nrow(att.mat) -1,
                          dimnames = list(rownames(att.mat[-1,]),
                                          paste0("att.rel.", colnames(att.mat))))
        att.abs <- matrix(NA, ncol = ncol(att.mat), nrow = nrow(att.mat) -1,
                          dimnames = list(rownames(att.mat[-1,]),
                                          paste0("att.abs.", colnames(att.mat))))

        for(i in 1: ncol(attribution)){
          row_sub <- att.mat[, i] != 0
          attribution[, i] <- att.mat[, i][row_sub]
          att.rel[,i] <- attribution[2: nrow(attribution), i]/attribution[2,i]
          att.abs[,i] <- attribution[2: nrow(attribution), i]/attribution[1,i]
        }

        att.final.rel <- matrix(att.rel[-1,], ncol = 1,
                                dimnames = list(paste0(rep(rownames(att.rel[-1,]), ncol(att.rel)),
                                                       ".", rep(colnames(att.mat), each = nrow(att.rel[-1,]))),
                                                "att.rel"))
        att.final.abs <- matrix(att.abs, ncol = 1, dimnames = list(paste0(rep(rownames(att.abs),
                                                                              ncol(att.abs)), ".", rep(colnames(att.mat),
                                                                                                       each = nrow(att.abs))),
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
      }

      if(attrib.disease == TRUE){
        for (i in 1:ncol(y.resp)){
          att.mat[[i]] <- matrix(NA, nrow = (ncol(x.mat) + 3), ncol = ncol(y.resp))
          att.mat[[i]][1,] <-  tapply(wgt[id.disab[[i]]], att.var.[id.disab[[i]]], sum)
          att.mat[[i]][2,] <-  tapply(pi_ij[[i]][id.disab[[i]]] * wgt[id.disab[[i]]],
                                      att.var.[id.disab[[i]]], sum)
          att.mat[[i]][3,] <-  tapply(att.back[[i]][id.disab[[i]]] * wgt[id.disab[[i]]],
                                      att.var.[id.disab[[i]]], sum)

          for (j in 1: ncol(x.mat)){
            att.mat[[i]][3 + j,] <- tapply(att.x[[i]][, j][id.disab[[i]]] * wgt[id.disab[[i]]],
                                           att.var.[id.disab[[i]]], sum)
          }

          dimnames(att.mat[[i]]) <- list(c("nn","disab","backgrnd", colnames(x.mat)),
                                         paste0("y", rep(colnames(y.resp)[i],
                                                         each = ncol(att.mat[[i]])), ".",
                                                "att.var", levels(att.var.)))
        }
        names(att.mat) <- paste0("y.level.", colnames(y.resp))

        att.mat1 <- list()
        for (i in 1:ncol(y.resp)){
          att.mat1[[i]] <- apply(att.mat[[i]], 1, sum)}

        if(collapse.background == FALSE){
          attribution <- rep(list(list()), ncol(y.resp))
          attribution2 <- rep(list(list()), ncol(y.resp))
          attribution3 <- list()
          att.rel <- list()
          att.abs <- list()
          names.att.rel <- list()
          names.att.abs <- list()

          for(i in 1: ncol(y.resp)){
            for(j in 1: ncol(att.mat[[1]])){
              attribution[[i]][[j]] <- att.mat[[i]][, j]
              row_sub <- attribution[[i]][[j]] != 0
              attribution2[[i]][[j]] <- attribution[[i]][[j]][row_sub]}}

          for(i in 1: ncol(y.resp)){
            attribution3[[i]] <- matrix(unlist(lapply(attribution2, `[`, i)),
                                        ncol = length(attribution2[[i]]),
                                        dimnames = list(c(names(attribution2[[i]][[i]])),
                                                        paste0("y", colnames(y.resp))))
          }
          names(attribution3) <- paste0("att.var.", levels(att.var.))

          for(i in 1: length(levels(att.var.))){
            names.att.rel[[i]] <- paste0(rep(rownames(attribution3[[i]][-c(1:2),]), ncol(attribution3[[i]][-c(1:2),])), ".",
                                         rep(levels(att.var.)[i], each = (nrow(attribution3[[i]]) - 2) * ncol(attribution3[[i]])), ".",
                                         rep(colnames(attribution3[[i]]), each = (nrow(attribution3[[i]]) - 2)))
            names.att.abs[[i]] <- paste0(rep(rownames(attribution3[[i]][-c(1),]), ncol(attribution3[[i]][-c(1),])), ".",
                                         rep(levels(att.var.)[i], each = (nrow(attribution3[[i]]) - 1) * ncol(attribution3[[i]])), ".",
                                         rep(colnames(attribution3[[i]]), each = (nrow(attribution3[[i]]) - 1)))
          }

          names.att.rel2 <- unlist(names.att.rel)
          names.att.abs2 <- unlist(names.att.abs)

          att.abs1 <- list()
          att.rel1 <- list()

          for (i in 1: length(attribution3)){
            att.abs1[[i]] <- matrix(t(apply(attribution3[[i]][2:nrow(attribution3[[i]]), ],
                                            1, "/", attribution3[[i]][1, ])), ncol = 1)
            att.rel1[[i]] <- matrix(t(apply(attribution3[[i]][3:nrow(attribution3[[i]]), ],
                                            1, "/", attribution3[[i]][2, ])), ncol = 1)
          }

          att.abs <- matrix(unlist(att.abs1),ncol = 1,
                            dimnames = list(c(names.att.abs2), "att.abs"))
          att.rel <- matrix(unlist(att.rel1),ncol = 1,
                            dimnames = list(c(names.att.rel2), "att.rel"))

          if(type.attrib == "rel"){
            att.final <-  att.rel
          }

          if(type.attrib == "abs"){
            att.final <- att.abs
          }

          if(type.attrib == "both"){
            att.final <- list(att.rel = att.rel, att.abs = att.abs)
          }
        }

        if(collapse.background == TRUE){
          attribution <- list()
          att.rel <- list()
          att.abs <- list()

          for(i in 1: ncol(y.resp)){
            row_sub <- att.mat1[[i]] != 0
            attribution[[i]] <- att.mat1[[i]][row_sub]}

          attribution2 <- matrix(unlist(attribution), ncol = length(attribution),
                                 dimnames = list(c(names(attribution[[1]])),
                                                 paste0("y", colnames(y.resp))))

          att.rel <- attribution2[3: nrow(attribution2),]/attribution2[2,]
          att.abs <- attribution2[2: nrow(attribution2),]/attribution2[1,]
          att.both <- list(att.rel, att.abs)
          names(att.both) <- c("att.rel", "att.abs")

          att.final.rel <- matrix(att.rel, ncol = 1,
                                  dimnames = list(c(paste0(rep(rownames(att.rel), ncol(att.rel)),
                                                           ".", rep(colnames(att.rel),
                                                                    each = nrow(att.rel)))), "att.rel"))

          att.final.abs <- matrix(att.abs, ncol = 1,
                                  dimnames = list(c(paste0(rep(rownames(att.abs), ncol(att.abs)),
                                                           ".", rep(colnames(att.abs),
                                                                    each = nrow(att.abs)))), "att.abs"))

          att.both <- list(att.rel = att.final.rel, att.abs = att.final.abs)

          if(type.attrib == "rel"){
            att.final <- att.final.rel
          }

          if(type.attrib == "abs"){
            att.final <- att.final.abs
          }

          if(type.attrib == "both"){
            att.final <- att.both
          }
        }
      }
    }
    Output <- att.final
    return(Output)
  }

  CovMultAddHaz <- function(start, x, y) {
    beta_ij <- matrix(unlist(split(start, cut(seq_along(start),
                                              ncol(y.resp), labels = FALSE))), ncol = ncol(y.resp))

    eta_ij <- matrix(NA, ncol = ncol(beta_ij), nrow = nrow(x))
    pi_ij <- matrix(NA, ncol = ncol(beta_ij), nrow = nrow(x))
    sum.y <- apply(y.resp, 1, sum)


    for (i in 1:ncol(beta_ij)) {
      eta_ij[, i] <- as.vector(x %*% beta_ij[, i])
    }

    sum.eta_ij <- apply(eta_ij, 1, sum)
    pi_i = 1 - exp(-(sum.eta_ij))
    for (i in 1:ncol(beta_ij)) {
      pi_ij[, i] <- pi_i * (eta_ij[, i]/sum.eta_ij)
    }
    sum.pi_ij <- apply(pi_ij, 1, sum)

    R = sum.y * ((1/(sum.eta_ij)^2) - ((exp(-sum.eta_ij))/(1 - exp(-sum.eta_ij))^2))
    R2 = (y.resp/(eta_ij)^2)

    djj = R - R2

    jj = vector("list", ncol(y.resp))
    for (i in 1:ncol(y.resp)) {
      jj[[i]] <- t(x) %*% diag(djj[, i]) %*% x
    }

    jjp = vector("list", ncol(y.resp))
    for (i in 1:ncol(y.resp)) {
      jjp[[i]] <- t(x) %*% diag(R) %*% x
    }

    if (ncol(y.resp) == 2) {
      cov1 <- cbind(jj[[1]], jjp[[1]])
      cov2 <- cbind(jjp[[2]], jj[[2]])
      vcov <- -1 * rbind(cov1, cov2)
    } else {
      cov.mat <- bdiag(jj)
      used <- 0
      a <- ncol(x)
      b <- ncol(y.resp)
      for (irow in 1:(b - 1)) {
        tempMat <- jjp[[used + 1]]
        if ((used + 1) != length(jjp)) {
          for (k in (used + 2):(used + b - irow)) {
            tempMat <- cbind(tempMat, jjp[[k]])
          }
        }
        rows <- seq((irow - 1) * a + 1, irow * a, length = a)
        columns <- seq(irow * a + 1, a * b, length = a *
                         b - irow * a)
        cov.mat[rows, columns] <- tempMat
        used <- used + b - irow
      }
      usedL <- 0
      for (irow in 2:b) {
        tempMat <- jjp[[usedL + 1]]
        if ((usedL + 1) > 1) {
          for (k in (usedL + 2):(usedL + 2 - b + irow)) {
            tempMat <- cbind(tempMat, jjp[[k]])
          }
        }
        rows <- seq((irow - 1) * a + 1, irow * a, length = a)
        columns <- seq(1, a * (irow - 1), length = a *
                         (irow - 1))
        cov.mat[rows, columns] <- tempMat
        usedL <- usedL + 2 - b + irow
      }
      vcov <- -1 * cov.mat
      return(vcov)
    }
  }

  ### Optimization
  sparse.mat <- paste0("bdiag(", paste0(rep("x,", ncol(y.resp)-1), collapse=""), "x)")
  ui.const <- as.matrix(eval(parse(text = sparse.mat)))
  MultLL <- constrOptim(theta = start, data = data, f = MultAddHazLogLik, ui = ui.const,
                        ci = rep(0, nrow(ui.const)), control = list(maxit = 1000),
                        method = "Nelder-Mead", y = y, x = x, wgt = w)

  Coeff <- matrix(unlist(split(MultLL$par, cut(seq_along(MultLL$par),
                                               ncol(y.resp)))), ncol = ncol(y.resp))
  colnames(Coeff) <- colnames(y.resp)
  rownames(Coeff) <- colnames(x)
  Coeff2 <- matrix(Coeff, ncol = 1)
  colnames(Coeff2) <- "Coefficients"
  rownames(Coeff2) <- paste0(colnames(x), ".y", rep(1:ncol(y.resp), each = ncol(x)))

  ### Attribution
  if(attrib){
    if (missing(attrib.var)){
      mult.coef <- Coeff
      Attribution <- AttribMult(mult.coef = mult.coef, attrib.coef = NULL,
                                data = data, y = y, y.resp = y.resp, x.mat = x,
                                att.var. = NULL, wgt = w, type.attrib = type.attrib,
                                collapse.background = FALSE, attrib.disease = FALSE)
    if (collapse.background == TRUE | attrib.disease == TRUE){
        stop("Cannot calculate contribution; attrib.var is missing")
      }
    } else {# If att.var is not missing
      att.var <- data[, deparse(substitute(attrib.var))]
      x.mat <- x[, -c(1:length(unique(att.var)))]
      att.var.levels <- sort(unique(att.var))
      attrib.length <- length(unique(att.var))
      if (!is.factor(att.var)) {
        att.var. <- factor(att.var, levels = att.var.levels,
                           labels = att.var.levels)
      }
      if (is.factor(att.var)) {
        att.var. <- att.var
      }

      if(attrib.length == 1){
        attrib.coef = matrix(Coeff[1, ], ncol = ncol(y.resp))
        mult.coef = Coeff[-1,]} else {
          attrib.coef = matrix(Coeff[1: attrib.length, ], ncol = ncol(y.resp))
          mult.coef = matrix(Coeff[-c(1: attrib.length), ], ncol = ncol(y.resp))
        }

      if (attrib.disease == FALSE){
        if (collapse.background == FALSE){
          Attribution <- AttribMult(mult.coef = mult.coef, attrib.coef = attrib.coef,
                                    data = data, y = y, y.resp = y.resp, x.mat = x.mat,
                                    att.var. = att.var., wgt = w, type.attrib = type.attrib,
                                    collapse.background = FALSE, attrib.disease = FALSE)
        } else {
          Attribution <- AttribMult(mult.coef = mult.coef, attrib.coef = attrib.coef,
                                    data = data, y = y, y.resp = y.resp, x.mat = x.mat,
                                    att.var. = att.var., wgt = w, type.attrib = type.attrib,
                                    collapse.background = TRUE, attrib.disease = FALSE)
        }
      }

      if(attrib.disease == TRUE){
        if (collapse.background == FALSE){
          Attribution <- AttribMult(mult.coef = mult.coef, attrib.coef = attrib.coef,
                                    data = data, y = y, y.resp = y.resp, x.mat = x.mat,
                                    att.var. = att.var., wgt = w, type.attrib = type.attrib,
                                    collapse.background = FALSE, attrib.disease = TRUE)
        }
        if (collapse.background == TRUE){
          Attribution <- AttribMult(mult.coef = mult.coef, attrib.coef = attrib.coef,
                                    data = data, y = y, y.resp = y.resp, x.mat = x.mat,
                                    att.var. = att.var., wgt = w, type.attrib = type.attrib,
                                    collapse.background = TRUE, attrib.disease = TRUE)
        }
      }
    }
  }

  ### CI
  if(bootstrap == FALSE){
    vcov <- CovMultAddHaz(start = Coeff, x = x, y = y)
    Invcov <- solve(vcov)
    stdError <- as.vector(sqrt(diag(Invcov)))

    Std <- matrix(unlist(split(stdError, cut(seq_along(stdError), ncol(y.resp)))),
                  ncol = ncol(y.resp))
    colnames(Std) <- colnames(y.resp)
    rownames(Std) <- colnames(x)

    CILow <- Coeff2 - (1.96 * stdError)
    CIHigh <- Coeff2 + (1.96 * stdError)
    pvalue <- round(2 * pnorm(-abs(Coeff/stdError)), 4)

    CI <- as.matrix(cbind(CILow, CIHigh), ncol = 2,
                    dimnames = list(paste0(colnames(x), rep(1:ncol(y.resp),
                                                            each = ncol(x)))))
    colnames(CI) <- c("CILow", "CIHigh")

    colnames(pvalue) <- colnames(pvalue)
    rownames(pvalue) <- colnames(x)

    if(attrib){
      Results <- list(coefficients = Coeff2, ci = CI,
                      resDeviance = MultLL$value,
                      df = nrow(data) - length(MultLL$par),
                      pvalue = matrix(pvalue, ncol = 1,
                                      dimnames = list(rownames(Coeff2), "p.value")),
                      stdError = matrix(Std, ncol = 1, dimnames = list(rownames(Coeff2), "StdErr")),
                      vcov = vcov, contribution = Attribution)
    } else {
    Results <- list(coefficients = Coeff2, ci = CI,
                    resDeviance = MultLL$value,
                    df = nrow(data) - length(MultLL$par),
                    pvalue = matrix(pvalue, ncol = 1,
                                    dimnames = list(rownames(Coeff2), "p.value")),
                    stdError = matrix(Std, ncol = 1, dimnames = list(rownames(Coeff2), "StdErr")),
                    vcov = vcov, contribution = attrib) }
    }

  ### bootstrap CI
  if(bootstrap == TRUE){
  if(attrib){
    if (missing(attrib.var)){
      mystat <- function(data, indices) {
        indices2 <- rep(indices, length(sort(unique(y))) - 1)
        m1 <-  constrOptim(theta = MultLL$par, data = data[indices,], f = MultAddHazLogLik,
                           ui = ui.const[indices2, ], ci = rep(0, nrow(ui.const)),
                           control=list(maxit=1000), method = "Nelder-Mead",
                           y = y[indices], x = x[indices,], wgt = w[indices])

        m2 <- AttribMult(mult.coef = mult.coef, attrib.coef = NULL,
                         data = data[indices,], y = y[indices], y.resp = y.resp[indices,],
                         x.mat = x[indices,],
                         att.var. = NULL, wgt = w[indices],
                         type.attrib = type.attrib, collapse.background = collapse.background,
                         attrib.disease = attrib.disease)

        if(is.list(m2)){
          m3 <- unlist(m2)} else {
            m3 <- m2}
        names(m1$par) <- rownames(Coeff2)
        res <- c(m1$par, m3)
        return(res)
      }
    } else {
  mystat <- function(data, indices) {
    indices2 <- rep(indices, length(sort(unique(y))) - 1)
    m1 <-  constrOptim(theta = MultLL$par, data = data[indices,], f = MultAddHazLogLik,
                      ui = ui.const[indices2, ], ci = rep(0, nrow(ui.const)),
                      control=list(maxit=1000), method = "Nelder-Mead",
                      y = y[indices], x = x[indices,], wgt = w[indices])

    m2 <- AttribMult(mult.coef = mult.coef, attrib.coef = attrib.coef,
               data = data[indices,], y = y[indices], y.resp = y.resp[indices,],
               x.mat = x.mat[indices,],
               att.var. = att.var.[indices], wgt = w[indices],
               type.attrib = type.attrib, collapse.background = collapse.background,
               attrib.disease = attrib.disease)

    if(is.list(m2)){
      m3 <- unlist(m2)} else {
      m3 <- m2}
    names(m1$par) <- rownames(Coeff2)
    res <- c(m1$par, m3)
    return(res)
  }}
} else {
  mystat <- function(data, indices) {
    indices2 <- rep(indices, length(sort(unique(y))) - 1)
    m1 <-  constrOptim(theta = MultLL$par, data = data[indices,], f = MultAddHazLogLik,
                       ui = ui.const[indices2, ], ci = rep(0, nrow(ui.const)),
                       control=list(maxit=1000), method = "Nelder-Mead",
                       y = y[indices], x = x[indices,], wgt = w[indices])

    return(m1$par)}
}

    if(!missing(seed)){
      set.seed(seed)}

  if (parallel == TRUE){
    BootResult <- boot(data = data, statistic = mystat, R = nbootstrap, parallel = type.parallel,
                       strata = y, ncpus = ncpus)} else {
    BootResult <- boot(data = data, statistic = mystat, R = nbootstrap, strata = y)
    }

  BootCI <- matrix(NA, ncol = 2, nrow = length(BootResult$t0))

  for (i in 1:length(BootResult$t0)){
    BootCI[i,] <- boot.ci(BootResult,  conf = conf.level, type = "perc", index = i)[[4]][, 4:5]}

  colnames(BootCI) <- c("CILow", "CIHigh")

  if(attrib){
    CILow.att <- matrix(BootCI[(length(start) + 1): length(BootResult$t0), 1], ncol = 1,
                        dimnames = list(c(rownames(Attribution)), "CILow"))

    CIHigh.att <- matrix(BootCI[(length(start) + 1): length(BootResult$t0), 2], ncol = 1,
                        dimnames = list(c(rownames(Attribution)), "CIHigh"))

    if(type.attrib == "both"){ # change here (Continue here!)
      att1 <- list(att.rel = cbind(Attribution[[1]], CILow.att[1: nrow(Attribution[[1]]),],
                                  CIHigh.att[1: nrow(Attribution[[1]]),]),
                   att.abs = cbind(Attribution[[2]],
                                  CILow.att[(nrow(Attribution[[1]]) + 1):
                                            (nrow(Attribution[[1]]) + nrow(Attribution[[2]])),],
                                  CIHigh.att[(nrow(Attribution[[1]]) + 1):
                                             (nrow(Attribution[[1]]) + nrow(Attribution[[2]])),]))
      colnames(att1[[1]]) <- c("att.rel", "CILow", "CIHigh")
      colnames(att1[[2]]) <- c("att.abs", "CILow", "CIHigh")
    } else {
        att1 <- cbind(Attribution, CILow.att, CIHigh.att)
    }

    Results <- list(coefficients = Coeff2,
                    ci = cbind(matrix(BootCI[1: length(start), "CILow"],
                    ncol = 1, dimnames = list(c(rownames(Coeff2)), "CILow")),
                    matrix(BootCI[1:length(start), "CIHigh"], ncol = 1,
                           dimnames = list(c(rownames(Coeff2)), "CIHigh"))),
                    resDeviance = MultLL$value, df = nrow(data) - length(MultLL$par),
                    contribution = att1, bootsRep = BootResult$t, conf.level = conf.level)
    if(class(Attribution) == "list"){
      colnames(Results$bootsRep) <- c(rownames(Coeff2), rownames(Attribution$att.rel), rownames(Attribution$att.abs))
    } else {
    colnames(Results$bootsRep) <- c(rownames(Coeff2), rownames(Attribution))}
    colnames(Results$ci) <- c("CILow", "CIHigh")

    } else {

    Results <- list(coefficients = Coeff2, ci = cbind(matrix(BootCI[1: length(start), "CILow"],
                                                             ncol = 1,
                                                      dimnames = list(rownames(Coeff2))),
                                                      matrix(BootCI[1:length(start), "CIHigh"], ncol = 1,
                                                      dimnames = list(rownames(Coeff2)))),
                      resDeviance = MultLL$value, df = nrow(data) - length(MultLL$par),
                      contribution = attrib, bootsRep = BootResult$t, conf.level = conf.level)

    colnames(Results$bootsRep) <- names(BootResult$t0)
    colnames(Results$ci) <- c("CILow", "CIHigh")}
  }

  Results$bootstrap <- bootstrap
  Results$call <- match.call()
  class(Results) <- "multaddhazmod"
  return(Results)
}

# GENERIC FUNCTIONS IN S3 LANGUAGE (PRINT, SUMMARY, PREDICT)
multaddhazmod <- function(x, ...) UseMethod("multaddhazmod")


print.multaddhazmod <- function(x,...){
  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(x$coefficients)}

summary.multaddhazmod <- function(object, ...){
  if(object$bootstrap == FALSE){
    se <- sqrt(diag(solve(object$vcov)))
    tval <- coef(object)/se
    TAB <- cbind(Estimate = as.vector(coef(object)),
                 StdErr = se,
                 t.value = as.vector(tval),
                 p.value = as.vector(2*pt(-abs(tval), df=object$df)))} else {
                   TAB <- cbind(Estimate = coef(object), CI = object$ci)}
    rownames(TAB) <- rownames(object$coefficients)

  if(object$bootstrap == FALSE){
    res <- list(call = object$call, bootstrap = object$bootstrap,
                coefficients = TAB)} else {
                  res <- list(call = object$call, bootstrap = object$bootstrap,
                              coefficients = TAB,
                              conf.level = object$conf.level)}
  class(res) <- "summary.multaddhazmod"
  res}

predict.multaddhazmod <- function(object, newdata = NULL, ...){
  if(is.null(newdata))
    y <- fitted(object)
  else{
    if(!is.null(object$formula)){
      x <- model.matrix(object$formula, newdata)
    } else{
      x <- newdata}
    y <- as.vector(x %*% coef(object))}
  y}

