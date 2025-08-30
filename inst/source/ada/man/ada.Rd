\name{ada}
\alias{ada}
\alias{ada.default}
\alias{ada.formula}
\alias{ada.logit}
\alias{ada.gentle}
\alias{ada.real}
\alias{ada.discrete}
\alias{ada.qtree1}
\alias{ada.qtree2}
\alias{ada.predst}

\title{
Fitting Stochastic Boosting Models
}

\description{
\sQuote{ada} is used to fit a variety stochastic boosting models for a binary response 
as described in \emph{Additive Logistic Regression:  A Statistical
View of Boosting} by Friedman, et al. (2000).
}
\usage{
ada(x,\dots)
\method{ada}{default}(x, y,test.x,test.y=NULL, loss=c("exponential","logistic"),
                      type=c("discrete","real","gentle"),iter=50, nu=0.1, bag.frac=0.5,
                      model.coef=TRUE,bag.shift=FALSE,max.iter=20,delta=10^(-10),
                      verbose=FALSE,\dots,na.action=na.rpart)

\method{ada}{formula}(formula, data, \dots, subset, na.action=na.rpart)

}

\arguments{
  \item{x}{matrix of descriptors.}
  \item{y}{vector of responses.  \sQuote{y} may have only two unique values.}
  \item{test.x}{testing matrix of discriptors (optional)}
  \item{test.y}{vector of testing responses (optional)}
  \item{loss}{ loss="exponential", "ada","e" or any variation corresponds to 
               the default boosting under exponential loss.  loss="logistic","l2","l"
               provides boosting under logistic loss.}
  \item{type}{type of boosting algorithm to perform.
              \dQuote{discrete} performs discrete Boosting (default).
              \dQuote{real} performs Real Boost.
              \dQuote{gentle} performs Gentle Boost.}
  \item{iter}{number of boosting iterations to perform.  Default = 50.}
  \item{nu}{shrinkage parameter for boosting, default taken as 1.}
  \item{bag.frac}{sampling fraction for samples taken out-of-bag.  This allows one
                  to use random permutation which improves performance.}
  \item{model.coef}{flag to use stageweights in boosting.  If FALSE then the procedure
                    corresponds to epsilon-boosting.}
  \item{bag.shift}{flag to determine whether the stageweights should go to 
                   one as nu goes to zero.  This only makes since if bag.frac
                   is small.  The rationale behind this parameter is discussed in
                   (Culp et al., 2006).}
  \item{max.iter}{number of iterations to perform in the newton step to determine 
                  the coeficient.}
  \item{delta}{tolarence for convergence of the newton step to determine 
                  the coeficient.}
  \item{verbose}{print the number of iterations necessary for convergence of a coeficient.}
  \item{formula}{a symbolic description of the model to be fit.}
  \item{data}{an optional data frame containing the variables in the model.}
  \item{subset}{an optional vector specifying a subset of observations to be
                used in the fitting process.}
  \item{na.action}{a function that indicates how to process \sQuote{NA} values.  Default=na.rpart.}
  \item{\dots}{arguments passed to \code{rpart.control}.  For stumps, use \code{rpart.control(maxdepth=1,cp=-1,minsplit=0,xval=0)}.
             \code{maxdepth} controls the depth of trees, and \code{cp}
	     controls the complexity of trees.  The priors should also
	     be fixed through the parms argument as discussed in the
	     second reference.}
}

\details{
This function directly follows the algorithms listed in \emph{\dQuote{Additive Logistic
Regression:  A Statistical View of Boosting}}.

When using usage \sQuote{ada(x,y)}:
  x data can take the form data.frame or as.matrix.
  y data can take form data.frame, as.factor, as.matrix, as.array, or as.table.
  Missing values must be removed from the data prior to execution.

When using usage \sQuote{ada(y~.)}:
  data must be in a data frame.  Response can have factor or numeric values.
  Missing values can be present in the descriptor data, whenever
  na.action is set to any option other than na.pass.
    
After the model is fit, \sQuote{ada} prints 
  a summary of the function call, 
  the method used for boosting, 
  the number of iterations,
  the final confusion matrix (observed classification vs predicted classification; 
  labels for classes are same as in response),  
  the error for the training set, and testing, training , and kappa estimates of the 
  appropriate number of iterations.

  A summary of this information can also be obtained with the command \sQuote{print(x)}.

  Corresponding functions (Use help with summary.ada, predict.ada, \dots
  varplot for additional information on these commands):

  summary :  function to print a summary of the original function call, method
                 used for boosting, number of iterations, final confusion matrix,
                 accuracy, and kappa statistic (a measure of agreement between
                 the observed classification and predicted classification).
                 \sQuote{summary} can be used for training, testing, or
		 validation data.  
		 

  predict :  function to predict the response for any data set (train,
  test, or validation).

  plot    :  function to plot performance of the algorithm across boosting iterations.
                 Default plot is iteration number (x-axis) versus prediction error (y-axis) for
                 the data set used to build the model.  Function can also simultaneously
                 produce an error plot for an external test set and a kappa plot for training and
                 test sets. 

  pairs   :  function to produce pairwise plots of descriptors.  Descriptors are arranged by 
                 decreasing frequency of selection by boosting (upper left = most frequently chosen).
                 The color of the marker in the plot represents class membership; the Size of the marker
                 represents predicted class probability.  The larger the marker, the higher the
                 probability of classification.

  varplot :  plot of variables ordered by the variable importance measure (based on improvement).

  addtest : add a testing data set to the \code{ada} object, therefore the testing errors only have to 
            be computed once.  

  update : add more trees to the \code{ada} object.
}

\value{
  \item{model}{
    The following items are the different components created by the algorithms:
    trees:  ensamble of rpart trees used to fit the model
    alpha:  the weights of the trees used in the final aggregate model (AdaBoost only; 
              see references for more information)
    F    :  F[[1]] corresponds to the training sum, F[[2]]], \dots corresponds to
            testing sums.
   errs  :  matrix of errs, training, kappa, testing 1, kappa 1, \dots
   lw    :  last weights calculated, used by update routine
  }
  \item{fit}{
    The predicted classification for each observation in the orginal level of the response.
  }
  \item{call}{
    The function call.
  }
  \item{nu}{shrinakge parameter}
  \item{type}{
    The type of adaboost performed:  \sQuote{discrete}, \sQuote{real}, \sQuote{logit}, and \sQuote{gentle}.
  }
  \item{confusion}{
    The confusion matrix (True value vs. Predicted value) for the training data.
  }
  \item{iter}{
    The number of boosting iterations that were performed.
  }
  \item{actual}{
    The original response vector.
  }
}

\references{
  Friedman, J. (1999). \emph{Greedy Function Approximation: A Gradient Boosting Machine.} 
    Technical Report, Department of Statistics, Standford University.

  Friedman, J., Hastie, T., and Tibshirani, R.  (2000).  \emph{Additive Logistic Regression:
    A statistical view of boosting}.  Annals of Statistics, 28(2), 337-374.

 Friedman, J. (2002). \emph{Stochastic Gradient Boosting}. 
   Coputational Statistics \& Data Analysis 38.

  Culp, M., Johnson, K., Michailidis, G. (2006). \emph{ada: an R Package
    for Stochastic Boosting} Journal of Statistical Software, 16.
}

\author{
Mark Culp, University of Michigan
Kjell Johnson, Pfizer, Inc.
George Michailidis, University of Michigan

Special thanks goes to:
Zhiguang Qian, Georgia Tech University
Greg Warnes, Pfizer, Inc.
}

\section{Warnings}{
  For LogitBoost and Gentle Boost, under certain circumstances, the
  methods will fail to classify the data into more than one category.
  If this occurs, try modifying the rpart.control options such as
  \sQuote{minsplit}, \sQuote{cp}, and \sQuote{maxdepth}.

  \sQuote{ada} does not currently handle multiclass problems.  However, there
  is an example in (Culp et al., 2006) that shows how to use this code
  in that setting.  Plots and other functions are not set up for this analysis.

}

\seealso{
  \code{\link{print.ada}},\code{\link{summary.ada}},\code{\link{predict.ada}}
  \code{\link{plot.ada}},\code{\link{pairs.ada}},\code{\link{update.ada}}
  \code{\link{addtest}}
}

\examples{
## fit discrete ada boost to a simple example
data(iris)
##drop setosa
iris[iris$Species!="setosa",]->iris
##set up testing and training data (60\% for training)
n<-dim(iris)[1]
trind<-sample(1:n,floor(.6*n),FALSE)
teind<-setdiff(1:n,trind)
iris[,5]<- as.factor((levels(iris[,5])[2:3])[as.numeric(iris[,5])-1])
##fit 8-split trees
gdis<-ada(Species~.,data=iris[trind,],iter=20,nu=1,type="discrete")
##add testing data set
gdis=addtest(gdis,iris[teind,-5],iris[teind,5])
##plot gdis
plot(gdis,TRUE,TRUE)
##variable selection plot
varplot(gdis)
##pairwise plot
pairs(gdis,iris[trind,-5],maxvar=2)

##for many more examples refer to reference (Culp et al., 2006)
}
\keyword{classes}
\keyword{models}
\keyword{methods}



