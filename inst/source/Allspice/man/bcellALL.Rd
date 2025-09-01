\name{bcellALL}
\alias{bcellALL}

\title{
Simulated data
}

\description{
Simulated data of B-cell acute lymphoblastic leukemia.
}

\usage{
bcellALL(n = 200, contamination = 0.05)
}

\arguments{
  \item{n}{
Number of samples.
  }
  \item{contamination}{
Proportion of samples with randomly shuffled values.
  }
}

\value{
Returns a list with two elements: \code{counts} contains gene RNA read
counts and \code{metadata} contains age and sex information and the
generating subtype label.
}
\examples{
# Simulate B-cell ALL samples.
simu <- bcellALL(5)
print(head(simu$counts))
print(simu$metadata)
}