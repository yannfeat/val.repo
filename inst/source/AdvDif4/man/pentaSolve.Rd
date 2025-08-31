\name{pentaSolve}
\alias{pentaSolve}
\title{Solving 'Ax=b' system}
\usage{pentaSolve(a1,a2,a3,a4,a5,b)}
\arguments{
  \item{a1}{A vector}
  \item{a2}{A vector}
  \item{a3}{A vector}
  \item{a4}{A vector}
  \item{a5}{A vector}
  \item{b}{A vector}
}
\value{
A vector with the x value
}
\description{This software solves an 'Ax=b' pentadiagonal system using a direct method. Variables a1, a2, a3, a4, a5 are matrix A diags and b is the vector.}
\examples{
#
# Solve a 'Ax=b' easy sample
#
a1<-c(1)
a2<-c(2,2)
a3<-c(7,7,7)
a4<-c(2,2)
a5<-c(1)
b<-c(11,12,13)
pentaSolve(a1,a2,a3,a4,a5,b)
}
