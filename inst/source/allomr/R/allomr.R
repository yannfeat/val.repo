allomr <- function(X, Y, X0, a, b){
  if (is.vector(Y)==TRUE){
    m1 <- lm(log(Y)~log(X))
    n <- length(X[!is.na(X)])
    if (missing(a)){
      a <- exp(m1$coefficients[1])
    }
    if (missing(b)){
      b <- m1$coefficients[2]
    }
    if (missing(X0)){
      X0 <- mean(X, na.rm=TRUE)
    }
    Yx <- Y*(X0/X)^b
    exp_e <- Y/(a*X^b)
    obj <-list(n, a, b, X0, Yx, exp_e)
    names(obj) <-c("n","a","b","X0","Yx","exp_e")
    class(obj) <- "allomr"
    return(obj)
  }
  else {
    if (!missing(a) | !missing(b)){
      message("NOTE: Y is a data set. Regression parameters will be derived from data.")
    }
    a<-NULL
    b<-NULL
    Yx<-NULL
    exp_e<-NULL
    for (i in 1:ncol(Y)){
      m1<- lm(log(Y[[i]])~log(X))
      n<-length(X[!is.na(X)])
      a<- cbind(a, exp(m1$coefficients[1]))
      b<- cbind(b, m1$coefficients[2])
      if (missing(X0)){
        X0<-mean(X, na.rm=TRUE)
      }
      Yx<- cbind(Yx, Y[[i]]*(X0/X)^m1$coefficients[2])
      exp_e<- cbind(exp_e, Y[[i]]/(exp(m1$coefficients[1])*X^m1$coefficients[2]))
    }
    obj <-list(n, a, b, X0, Yx, exp_e)
    names(obj) <-c("n","a","b","X0","Yx","exp_e")
    class(obj) <- "allomr"
    return(obj)
  }
}
