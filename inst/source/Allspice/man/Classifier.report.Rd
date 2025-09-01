\name{report,Classifier-method}
\alias{report,Classifier-method}
\alias{report}

\title{
Visualization of results
}

\description{
Creates a visual report of the classification results.
}

\usage{
report(obj, name, file = NULL)
}

\arguments{
  \item{obj}{
An object of the class Classifier.
  }
  \item{name}{
Name of the sample to be shown.
  }
   \item{file}{
Name of the output file.
  }
}

\details{
The function generates a Scalable Vector Graphics figure that shows the
results from each classification asset within the Classifier. The report
will highlight the predicted category label and quality metrics for the
primary asset and bar charts for the fits to categories in all assests.

If no file name is provided, the report is plotted on the current device,
however, note that best visual outcomes are achieved by plotting in a file,
especially with classifiers with three or more assets.
}

\value{
Returns the name of the output file.
}

\examples{
# Simulated data.
simu <- bcellALL(5)
keys <- colnames(simu$counts)

# Predict subtypes.
cls <- classifier(verbose = FALSE)
covariates(cls) <- simu$metadata
profiles(cls) <- simu$counts

# Show visual report by name.
dev.new()
report(cls, name = keys[3])

# Show visual report by sample index.
dev.new()
report(cls, name = 3)
}