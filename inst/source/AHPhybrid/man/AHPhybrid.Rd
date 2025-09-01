\name{AHPhybrid}

\alias{AHPhybrid}

\title{AHP Hybrid}

\usage{ AHPhybrid(title, Alternatives, Qualitative_criteria, Quantitative_criteria,
Quantitative_crit_min_max, n_alt, n_crit, n_crit_Qual, n_crit_Quant, Criteria_Comparison,
Alternatives_comparison_qualit_crit, Alternatives_quantitative_crit)
}

\description{The AHP method (Analytic Hierarchy Process) is a multi-criteria decision-making method addressing choice and outranking problems. The method enables to perform the analysis of alternatives in each type of criterion and then provides a global performance of each alternative in the decision context. The main difference of this package is the possibility of evaluating the alternatives using quantitative data, by numerical representation, and qualitative data, using the Saaty scale, providing preference relation between variables by a pairwise evaluation.}

\arguments{

\item{title}{Title of analysis.}

\item{Alternatives}{Name of alternatives in analysis.}

\item{Qualitative_criteria}{Name of criteria with qualitative performance.}

\item{Quantitative_criteria,}{Name of criteria with quantitative performance.}

\item{Quantitative_crit_min_max}{A vector with objectives, minimize or maximize, to each criteria.}

\item{n_alt}{number of alternatives (It is not necessary to  make any input).}

\item{n_crit}{number of criteria (It is not necessary to  make any input).}

\item{n_crit_Qual}{number of qualitative criteria (It is not necessary to  make any input).}

\item{n_crit_Quant}{number of quantitative criteria (It is not necessary to  make any input).}

\item{Criteria_Comparison}{Input of matrix comparison with the preferences relations between the criteria.}

\item{Alternatives_comparison_qualit_crit}{Input of matrix comparison with the preferences relations between the alternatives in each qualitative criterion.}

\item{Alternatives_quantitative_crit}{Input of quantitative performance of alternatives in each quantitative criterion.}

}

\value{

- Calculation of criteria priorities;

- Calculation of alternatives priorities in each criterion and in  a global context.

- Validadtion of prefernce inputs by the consistency index;

- Provide the analysis considering qunatitative and qualitative data.

}

\references{
GOLDEN, Bruce L.; WASIL, Edward A.; HARKER, Patrick T. The analytic hierarchy process. Applications and Studies, Berlin, Heidelberg, 1989. <https://link.springer.com/book/10.1007%2F978-3-642-50244-6>
}

\author{Miguel Angelo Lellis Moreira \email{miguellellis@hotmail.com},
        Marcos dos Santos \email{marcosdossantos_doutorado_uff@yahoo.com.br},
        Carlos Francisco Simoes Gomes \email{cfsg1@bol.com.br},
        Claudio de Souza Rocha junior \email{claudiodesouzarochajunior@gmail.com}}



\examples{
title <- "Layout Manufacturing Analysis"

Alternatives <- c("Layout_A", "Layout_B")

Qualitative_criteria <- c("Material  Handling","Process Quality" )

Quantitative_criteria <- c("Cost", "Production", "Cost Reduction")

Quantitative_crit_min_max <- c("min","max", "max")

n_alt <- length(Alternatives)
n_crit <- (length(Qualitative_criteria)+length(Quantitative_criteria))
n_crit_Qual <- length(Qualitative_criteria)
n_crit_Quant <- length(Quantitative_criteria)


Criteria_Comparison <- matrix(c(1, 1/5,  1, 1/7, 1/2,
                                5,   1,  3,   1,   2,
                                1, 1/3,  1, 1/4, 1/2,
                                7,   1,  4,   1,   2,
                                2, 1/2,  2, 1/2,   1),

                                ncol = n_crit, nrow = n_crit, byrow = TRUE)


Alternatives_comparison_qualit_crit <- list(

                                            matrix(c(  1,   5,
                                                     1/5,   1),

                                                   ncol = n_alt, nrow = n_alt, byrow = TRUE),

                                            matrix(c(  1, 1/3,
                                                       3,   1),

                                                   ncol = n_alt, nrow = n_alt, byrow = TRUE)

                                            )

Alternatives_quantitative_crit <- matrix(c(200000, 17389, 420000,
                                           300000, 28257, 360000),

                                         nrow = n_alt, ncol = n_crit_Quant, byrow = TRUE)


AHPhybrid(title, Alternatives, Qualitative_criteria, Quantitative_criteria,
Quantitative_crit_min_max, n_alt, n_crit, n_crit_Qual, n_crit_Quant, Criteria_Comparison,
Alternatives_comparison_qualit_crit, Alternatives_quantitative_crit)




}
