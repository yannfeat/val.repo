paep<-function(x, alpha, sigma, mu, epsilon, log.p = FALSE, lower.tail = TRUE)
{
  n<-length(x)
  cdf0<-rep(NA, n)
  for (i in 1:n)
  {
    if (x[i]>  mu){ cdf0[i] <- 1-((1+epsilon)/2-(1+epsilon)/2*pgamma((x[i]-mu)^alpha/(sigma*(1+epsilon))^alpha,1/alpha,1)) }
    if (x[i]<= mu){ cdf0[i] <-   ((1-epsilon)/2-(1-epsilon)/2*pgamma((mu-x[i])^alpha/(sigma*(1-epsilon))^alpha,1/alpha,1)) }
  }
  if(log.p==TRUE  & lower.tail == FALSE) cdf0<-suppressWarnings( log(1-cdf0) )
  if(log.p==TRUE  & lower.tail == TRUE)  cdf0<-suppressWarnings( log(cdf0) )
  if(log.p==FALSE & lower.tail == FALSE) cdf0<-suppressWarnings( 1-cdf0 )
return(cdf0)
}


daep<-function(x, alpha, sigma, mu, epsilon, log = FALSE)
{
  n<-length(x)
  pdf0<-rep(NA, n)
    for(i in 1:n)
    {
      pdf0[i]<-1/(2*sigma*gamma(1+1/alpha))*exp(-(abs(x[i]-mu)/(sigma*(1+sign(x[i]-mu)*epsilon)))^alpha)
    }
      suppressWarnings(if(log==TRUE) pdf0<-log(pdf0))
return(pdf0)
}


raep<-function(n, alpha, sigma, mu, epsilon)
{
  sab<-function(n, a, beta)
  {
    y <- c()
    m <- c()
    b <- beta/a
    i <- 1
    bbb <- a^(-a)*(1-a)^(-(1-a))
    si <- 1/sqrt(b*a*(1-a))
    while(i<=n)
    {
      if (si>=sqrt(2*pi))
      {
        uu<-runif(1,0,pi)
        vv<-runif(1,0,1)
        if( (vv*bbb^b)<((sin(uu)/((sin(a*uu))^(a)*(sin((1-a)*uu))^(1-a)))^b) )
        {
          y[i]<-uu
          i<-i+1
        }
      }
      else
      {
        N<-rnorm(1,0,1)
        vv<-runif(1,0,1)
        if( ((si*abs(N))<pi) && (vv*bbb^b*exp(-N^2/2))<((sin(si*abs(N))/((sin(a*si*abs(N)))^(a)*(sin((1-a)*si*abs(N)))^(1-a)))^b) )
        {
          y[i]<-si*abs(N)
          i<-i+1
        }
      }
    }
    m<-(((sin(a*y))^(a)*(sin((1-a)*y))^(1-a))/sin(y))^(1/(1-a))
    return(m)
  }
  W <- (sab(n,alpha/2,.5)/rgamma(n,(1+1*(1-alpha/2)/alpha),1))^(2*(1-alpha/2)/alpha)
  rb<- rbinom(n,1,(1-epsilon)/2)
  X <- (1-rb)*(1+epsilon)*abs(rnorm(n))-(rb)*(1-epsilon)*abs(rnorm(n))
  Y <- sigma*(X)/sqrt(2*W)+mu
  return(Y)
}

qaep<-function(u, alpha, sigma, mu, epsilon)
{
  n<-length(u)
  R<-rep(NA, n)
  for (i in 1:n)
  {
    if ( u[i]<(1-epsilon)/2 )
    {
      R[i] <- mu - sigma*(1-epsilon)*( qgamma(1-2*u[i]/(1-epsilon), 1/alpha, 1) )^(1/alpha)
    }else{
      R[i] <- mu + sigma*(1+epsilon)*( qgamma((u[i]-(1-epsilon)/2)*2/(1+epsilon), 1/alpha, 1) )^(1/alpha)
    }
  }
  return(R)
}
fitaep<-function(x, initial = FALSE, starts)
{
  N  <- 6000
  cri<- 10e-5
  j  <- 2
  Eps<- 1
  del<- 10e-6
  n  <- length(x)
  E  <- anderson <- u<- von<- rep(NA,n)
  A  <- matrix(NA, nrow=N, ncol=4)
  OFIM <- matrix(NA, nrow=4, ncol=4)
  if(initial==FALSE)
  {
    f0.alpha <- function(w){-mean((x-mean(x))^4)/((n-1)/n*var(x))^2+gamma(5/w)*gamma(1/w)/(gamma(3/w))^2}
    alpha <- ifelse( f0.alpha(0.05)*f0.alpha(2)<0,uniroot(f0.alpha, lower=0.05, upper=2)$root, 1 )
    mu <- median(x)
    epsilon <- 1-2*sum( ifelse( (x-mu)<0, 1, 0) )/n
    sigma <- sqrt(var(x)*gamma(1/alpha)/gamma(3/alpha))
  }
  if(initial==TRUE)
  {
    alpha <- starts[1]
    sigma <- starts[2]
    mu <- starts[3]
    epsilon <- starts[4]
  }
  A[1,] <- c(alpha, sigma, mu, epsilon)
  while ( Eps>0.5 & j<N )
  {
    E  <- ifelse( abs(x-mu)<=0.00000001, mu, alpha/2*(abs(x-mu)/sigma)^(alpha-2))*abs(1+sign(x-mu)*epsilon)^(2-alpha )
    mu  <- sum( x*E/(1+sign(x-mu)*epsilon)^2 )/sum( E/(1+sign(x-mu)*epsilon)^2 )
    sigma  <- (2/n*sum( (x-mu)^2*E/(1+sign(x-mu)*epsilon)^2) )^(1/2)
    F  <- function(par) sum( (x-mu)^2*E/( sigma^2*(1 + sign(x-mu)*par[1])^2 ) )
    epsilon  <- optimize(F, c(-0.999,0.999) )$minimum
    f  <- function(par) n*lgamma(1+1/par[1])+sum( (abs(x-mu)/(sigma*(1+sign(x-mu)*epsilon)) )^par[1])
    alpha <- optimize(f, c(.01,2) )$minimum
    A[j,]  <-  c(alpha, sigma, mu, epsilon)
    #print(c(j,A[j,]))
    if ( sum( abs(A[j-1,]-A[j,]) )<cri || j>=(N-1) )
    {
      Eps<-0
    }else{
      j<-j+1
    }
  }
  alpha   <- A[j,1]
  sigma   <- A[j,2]
  mu      <- A[j,3]
  epsilon <- A[j,4]

  D <-cbind(
    ( digamma(1+1/alpha)/(alpha^2)-(abs(x-mu)/(sigma*(1+sign(x-mu)*epsilon) ) )^(alpha)*log( (abs(x-mu)/(sigma*(1+sign(x-mu)*epsilon) ) ) ) ),
    ( -1/sigma+ alpha*sigma^(-alpha-1)*( abs(x-mu)/ (1+sign(x-mu)*epsilon)  )^alpha ),
    alpha*sign(x-mu)/( sigma*(1+ sign(x-mu)*epsilon) )* (abs(x-mu)/(sigma*(1+sign(x-mu)*epsilon) ) )^(alpha-1),
    alpha*sign(x-mu)/( 1+ sign(x-mu)*epsilon )*(abs(x-mu)/(sigma*(1+sign(x-mu)*epsilon) ) )^(alpha)
  )
  OFIM <- solve(t(D)%*%D)
  s.Y <- sort(x)
  cdf0 <- paep(s.Y, alpha, sigma, mu, epsilon)
  pdf0 <- daep(s.Y, alpha, sigma, mu, epsilon)
  for(i in 1:n)
  {
    u[i] <- ifelse( cdf0[i]==1, 0.99999999, cdf0[i] )
    von[i] <- ( cdf0[i]-(2*i-1)/(2*n) )^2
    anderson[i] <- suppressWarnings( (2*i-1)*log(cdf0[i])+(2*n+1-2*i)*log(1-cdf0[i]) )
  }
  von.stat <- suppressWarnings( sum(von)+1/(12*n) )
  n.p <- 3
  log.likelihood <- suppressWarnings( sum( log(pdf0) ) )
  I <- seq(1,n)
  ks.stat <- suppressWarnings( max( I/n-cdf0, cdf0-(I-1)/n ) )
  anderson.stat <- suppressWarnings( -n - mean(anderson) )
  CAIC <- -2*log.likelihood + 2*n.p + 2*(n.p*(n.p+1))/(n-n.p-1)
  AIC  <- -2*log.likelihood + 2*n.p
  BIC  <- -2*log.likelihood + n.p*log(n)
  HQIC <- -2*log.likelihood + 2*log(log(n))*n.p
  out1 <- cbind(alpha, sigma, mu, epsilon)
  out2 <- cbind(AIC, CAIC, BIC, HQIC, anderson.stat, von.stat, ks.stat, log.likelihood)
  colnames(out1) <- c("alpha", "sigma", "mu", "epsilon")
  colnames(out2) <- c("AIC", "CAIC", "BIC", "HQIC", "AD", "CVM", "KS", "log.likelihood")
  out3 <- OFIM
  colnames(out3) <- c("alpha","sigma", "mu", "epsilon")
  rownames(out3) <- c("alpha","sigma", "mu", "epsilon")
  list("estimate" = out1, "measures" = out2, "Inverted OFIM" = out3)
}

regaep<-function(y, x){
  if( any(is.na(y)) ) warning('y contains missing values')
  if( any(is.na(x)) ) warning('x contains missing values')
  n     <- length(y)
  regout<-function(Y, X){
    n     <- length(Y)
    m2    <- 0.8
    m1    <- 0.2
    N     <- 2000
    cri   <- 10e-5
    j     <- 2
    Eps   <- 1
    u     <- rep( NA, n )
    y     <- rep( NA, n )
    k     <- dim( cbind(Y, X) )[2]
    Y1    <- matrix( NA, nrow = n, ncol = k)
    OFIM  <- matrix( NA, k, k)
    x     <- cbind( rep(1, n), X )
    out   <- matrix( NA, ncol = (3+k), nrow = N )
    X1    <- subset( cbind(Y,X), (Y<quantile(Y,0.8) | Y>quantile(Y,0.2)) )
    Beta  <- summary(lm( X1[,1]~X1[,2], data=data.frame(X1) ))$coefficients[1:k]
    f0.alpha <- function(x){-mean((Y-mean(Y))^4)/((n-1)/n*var(Y))^2+gamma(5/x)*gamma(1/x)/(gamma(3/x))^2}
    alpha    <- ifelse(f0.alpha(0.05)*f0.alpha(2)<0, uniroot(f0.alpha, lower=0.05, upper=2)$root, 1)
    epsilon  <- (quantile(Y,m2)[[1]]-2*quantile(Y,.5)[[1]]+quantile(Y,m1)[[1]])/(quantile(Y,m2)[[1]]-quantile(Y,m1)[[1]])
    mu       <- quantile(Y,(1-(epsilon))/2)[[1]]
    sigma    <- sqrt(var(Y)*gamma(1/alpha)/gamma(3/alpha) )
    out[1,1:k] <- Beta
    out[1,(k+1):(k+3)] <- c(alpha, sigma, epsilon)

    while (Eps>0.5 & j<N)
    {
      y <- Y-x%*%Beta
      E <- ifelse( abs(y-mu)<=0.00000001, mu, alpha/2*(abs(y-mu)/sigma)^(alpha-2))*abs(1+sign(y-mu)*epsilon)^(2-alpha )
      mu <- 0 # sum( y*E/(1+sign(y-mu)*epsilon)^2 )/sum( E/(1+sign(y-mu)*epsilon)^2 )
      sigma <- (2/n*sum( (y-mu)^2*E/(1+sign(y-mu)*epsilon)^2) )^(1/2)
      F  <- function(par) sum( (y-mu)^2*E/( sigma^2*(1 + sign(y-mu)*par[1])^2 ) )
      epsilon <- optimize(F, c(-0.999,0.999) )$minimum
      f <- function(par) n*lgamma(1+1/par[1])+sum( (abs(y-mu)/(sigma*(1+sign(y-mu)*epsilon)) )^par[1])
      alpha <- optimize(f, c(.01,2) )$minimum
      T <- matrix(0, k, k)
      Y1 <- t(x)%*%( (Y-mu)*E/(1+sign(y-mu)*epsilon)^2 )
      for (i in 1:n)
      {
        T <- T+(x[i,])%*%t(x[i,])*(E[i]/(1+sign(y[i]-mu)*epsilon)^2)[1]
      }
      Beta <- as.vector( solve(T)%*%Y1 )
      out[j,] <- c(Beta, alpha, sigma, epsilon )
      #	print(c(j,out[j,]))
      if (sum(abs(out[j-1,1:(k+3)]-out[j,1:(k+3)]))<cri || j>=(N-1))
      {
        Eps <- 0}else{
          j   <- j+1
        }
    }
    list( Beta = Beta, alpha = alpha, sigma = sigma, epsilon = epsilon )
  }
  out<-regout(y, x)

    w     <- y-cbind(1,x)%*%out$Beta
    alpha <- out$alpha
    sigma <- out$sigma
  epsilon <- out$epsilon
        p <- length(out$Beta)

  D<-cbind(  cbind(1,x)*
      matrix( rep( alpha*sign(w)/( sigma*(1+ sign(w)*epsilon) )*(abs(w)/( sigma*(1+sign(w)*epsilon) ) )^(alpha-1), p ), nrow = n, ncol = p ),
      digamma(1+1/alpha)/(alpha^2)-(abs(w)/(sigma*(1+sign(w)*epsilon) ) )^(alpha)*log( (abs(w)/(sigma*(1+sign(w)*epsilon) ) ) ),
      ( -1/sigma+ alpha*sigma^(-alpha-1)*( abs(w)/( (1+sign(w)*epsilon) ) )^(alpha) ),
      alpha*sign(w)/( 1+ sign(w)*epsilon )*(abs(w)/(sigma*(1+sign(w)*epsilon) ) )^(alpha)
     )

  colnames(D) <- NULL
  OFIM <- solve( t(D)%*%D )

  Error <- w
  S.E   <- sum( Error^2 )
  S.T   <- sum( (y-mean(y))^2 )
  F.value <- (S.T-S.E)/(p-1)*(n-p)*S.E

  out1 <- cbind(out$Beta)
  colnames(out1) <- c("Estimate")
  rownames(out1) <- c("beta.0", rownames(out1[2:p,], do.NULL = FALSE, prefix = "beta.") )

  out2 <- cbind( min(Error), quantile(Error,0.25)[[1]], quantile(Error,0.50)[[1]], mean(Error), quantile(Error,0.75)[[1]], max(Error) )
  colnames(out2) <- c("Min", "1Q", "Median", "Mean", "3Q", "Max")

  out3 <- cbind( F.value, p-1, n-p, 1-pf(F.value, p-1, n-p) )
  colnames(out3) <- cbind("Value", "DF1", "DF2", "P value")
  rownames(out3) <- c("F-statistic")

  out4 <- cbind(out$sigma, p-1)
  colnames(out4) <- cbind("Value", "DF")
  rownames(out4) <- c("Residual Std. Error")

  out5 <- cbind( 1-S.E/S.T, 1-(n-1)/(n-p)*(S.E/S.T) )
  colnames(out5) <- cbind("Non-adjusted", "Adjusted")
  rownames(out5) <- c("Multiple R-Squared")

  out6 <- cbind(out$alpha, out$sigma, out$epsilon)
  colnames(out6) <- c("Tail thickness", "Scale", "Skewness")

  colnames(OFIM) <- NULL
  out7 <- OFIM
  colnames(out7) <- c("beta.0",colnames(out7[,2:p], do.NULL = FALSE, prefix = "beta."), "alpha", "sigma", "epsilon")
  rownames(out7) <- c("beta.0",colnames(out7[,2:p], do.NULL = FALSE, prefix = "beta."), "alpha", "sigma", "epsilon")

  list("Coefficients:" = out1,
       "Residuals:" = out2,
       "F:" = out3,
       "MSE:" = out4,
       "R2:" = out5,
       "Estimated Parameters for Error Distribution:" = out6,
       "Inverted Observed Fisher Information Matrix:" = out7)
}
