\name{polarisation.EGR}
\alias{polarisation.EGR}

\title{ Polarisation Measure from Esteban, Gradin and Ray (2007)
}
\description{ This function computes the polarisation measure proposed in Esteban, Gradin and Ray (2007) 
which accounts for deviations from an n-spike representation of strata in society. 
}
\usage{
polarisation.EGR(alpha, beta, rho, y, f = NULL, dist = NULL, 
weights = NULL, pm0 = NA, lower = NULL, upper = NULL, ...)
}
\arguments{
  \item{alpha}{ a scalar containing the alpha parameter from Esteban and Ray (1994) on the sensitivity to polarisation.
}
  \item{beta}{ a scalar containing the beta parameter from Esteban, Gradin and Ray (2007) on the weight assigned to the error in the n-spike representation. 
}
  \item{rho}{ a dataframe with the group means in the first column and their respective population shares in the second. 
  The groups need to be exogenously defined. 
  
  Note: the two columns should be named \code{means} and \code{shares} respectively. Otherwise a warning will appear. 
}
  \item{y}{ a vector of incomes. If f is NULL and dist is NULL, this includes all incomes of all observations in the sample, 
  i.e. all observations comprising the aggregate distribution. 
  If either f or dist is not NULL, then this gives the incomes where the density is evaluated.
}
  \item{f}{ a vector of user-defined densities of the aggregate distribution for the given incomes in y. 
}
  \item{dist}{ character string with the name of the distribution used. Must be equivalent to the respective function of that distribution, e.g. norm for the normal distribution.
}
  \item{weights}{ an optional vector of weights to be used in the fitting process. Should be NULL or a numeric vector. If non-NULL, observations in y are weighted accordingly.
}
  \item{pm0}{ the point mass for zero incomes used in the gini.den function. If not specified no point mass is assumed.
}
  \item{lower}{ the lower bound of the income range considered used in the gini.den function. 
}
  \item{upper}{ the upper bound of the income range considered used in the gini.den function. 
}
  \item{\dots}{ arguments to be passed to the distribution function used, e.g. mean and sd for the normal distribution. 
}
}
\value{
\item{P}{the polarisation measure proposed by Esteban, Gradin and Ray (2007).}
\item{PG}{the adjusted polarisation measure proposed by Gradin (2000).}
\item{alpha}{the alpha parameter used.}
\item{beta}{the beta parameter used.}
\item{beta}{the distribution option used, i.e. whether only y, f or dist was used.}
}
\references{
Esteban, J. and Ray, D. (1994): On the Measurment of Polarization, in: Econometrica, Vol. 62(4), pp. 819-851.

Esteban, J., Gradin, C. and Ray, D. (2007): Extensions of a Measure of Polarization, with an Application to the Income Distribution of five OECD Countries.

Gradin, C. (2000): Polarization by Sub-populations in Spain, 1973-91, in Review of Income and Wealth, Vol. 46(4), pp.457-474.
}
\author{ Alexander Sohn
}

\seealso{\code{\link{polarisation.ER}}
}
\examples{
## example 1
y<-rnorm(1000,5,0.5)
y<-sort(y)
m.y<-mean(y)
sd.y<-sd(y)
y1<-y[1:(length(y)/4)]
m.y1<-mean(y1)
sd.y1<-sd(y1)
y2<-y[(length(y)/4+1):length(y)]
m.y2<-mean(y2)
sd.y2<-sd(y2)
means<-c(m.y1,m.y2)
share1<- length(y1)/length(y)
share2<- length(y2)/length(y)
shares<- c(share1,share2)
rho<-data.frame(means=means,shares=shares)
alpha<-1
beta<-1
den<-density(y)
polarisation.ER(alpha,rho,comp=FALSE)
polarisation.EGR(alpha,beta,rho,y)$P
polarisation.EGR(alpha,beta,rho,y=den$x,f=den$y)$P
polarisation.EGR(alpha,beta,rho,y=seq(0,10,by=0.1),dist="norm",
mean=m.y,sd=sd.y)$P
polarisation.EGR(alpha,beta,rho,y=seq(0,10,by=0.1),dist="norm",
mean=m.y,sd=sd.y)$PG

## example 2
y1<-rnorm(100,5,1)
y2<-rnorm(100,1,0.1)
y <- c(y1,y2)
m.y1<-mean(y1)
sd.y1<-sd(y1)
m.y2<-mean(y2)
sd.y2<-sd(y2)
means<-c(m.y1,m.y2)
share1<- length(y1)/length(y)
share2<- length(y2)/length(y)
shares<- c(share1,share2)
rho<-data.frame(means=means,shares=shares)
alpha<-1
beta<-1
polarisation.EGR(alpha,beta,rho,y=seq(0,10,by=0.1),dist="norm",
                 mean=c(m.y1,m.y2),sd=c(sd.y1,sd.y2))$P

}