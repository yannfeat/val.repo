## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ahptopsis2n)

# define the decision matrix
decision<-matrix(c(1100, 5, 25,
                   850, 3.5, 10,
                   950, 4, 30), ncol=3, byrow=TRUE)

rownames(decision)<- c("A1", "A2", "A3")

#define criteria matrix with pairwise comparison
criteria<-matrix(c(1, 1, 3,
                   1, 1, 5,
                   1/3, 1/5, 1), ncol=3, byrow=TRUE)

# define each criterion objective
minmax<-c("min", "max", "min")

# associate the objects to the function arguments and run the function
ahptopsis2n(decision=decision, criteria=criteria, minmax=minmax)


