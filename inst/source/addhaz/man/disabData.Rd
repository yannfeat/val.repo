\name{disabData}
\alias{disabData}

\title{Example of disability data}

\description{
The disabData is a subset of the data from the 2013 National Health Survey in Brazil ("Pesquisa Nacional de Saude, 2013"). The data are restricted to women aged 60 years or older, resulting in 6294 individuals.
}

\usage{
data(disabData)
}

\format{
This dataset has information about disability and chronic conditions. The disability outcomes were defined as limitations on instrumental activities of daily living (IADL). Individuals with missing data were excluded. The data frame contains 7 variables:
\itemize{
  \item \bold{dis.bin:} disability as a binary variable, with 2 categories: 0 (no disability),
  1 (disability).
    \item \bold{dis.mult:} disability as a multinomial variable, with 3 categories: 0 (no disability), 1 (mild disability), and 2 (severe disability).
  \item \bold{wgt:} survey weights.
  \item \bold{age:} binary variable for age: 0 (60-79 years) or 1 (80+ years).
  \item \bold{diab:} binary variable for diabetes: 0 (no) or 1 (yes).
  \item \bold{arth:} binary variable for arthritis: 0 (no) or 1 (yes).
  \item \bold{stro:} binary variable for stroke: 0 (no) or 1 (yes).
  }
}

\source{
The data were obtained from the National Health Survey 2013, Brazil. For more information about the survey, see references.
}

\references{
Szwarcwald, C.L., Malta, D.C., Pereira, C.A., Vieira, M.L., Conde, W.L., Souza Junior, P.R., et al. (2013). National Health Survey in Brazil: design and methodology of application. Cien Saude Colet., 19(2): 333:42 [Article in Portuguese].

Instituto Brasileiro de Geografia e Estatistica (IBGE). Pesquisa Nacional de Saude 2013. Available at <\url{http://www.ibge.gov.br/home/estatistica/populacao/pns/2013/}>.
}

\examples{
  data(disabData)
  str(disabData)
}

\keyword{ ~datasets }
\keyword{ ~survey }
