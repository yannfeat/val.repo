## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(affinity)

## ----adjacency----------------------------------------------------------------
 (m <- matrix(1:12, 3))
 tl(m)
 tr(m)
 bl(m)
 br(m)
 tl(br(m))
 image0(tl(br(m)))
 text0(tl(br(m)))

n <- 8
nbr <- 2
line <- seq(-nbr, nbr)
line + n
 
## we have values 1:12
## if we want the topleft neighbour in a *corner* orientation
matrix(c(tl(m), tr(m), bl(m), br(m)), ncol = 4L)
## see that the topleft corner of 1 (find 1 in the first column, row 4) 
# has values 1 and 4 (there's no column to their left)

## the topleft corner of 5 has values 4, 5, 7, 8 (row 10 in the print out)


