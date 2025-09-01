## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval = FALSE-------------------------------------------------------------
#  # install.packages("devtools")
#  devtools::install_github("el-meyer/airship@*release")

## ----eval = FALSE-------------------------------------------------------------
#  library(airship)
#  airship()

## ----echo = FALSE, warning = FALSE, message = FALSE---------------------------
DT::datatable(
  head(airship::ExampleData1),
    filter = "bottom",
    class = 'cell-border stripe',
    extensions = c('Scroller', 'FixedColumns'),
    options = list(
      dom = 'Bfrtip',
      scrollX = TRUE,
      fixedColumns = TRUE,
      deferRender = TRUE,
      scrollY = 200,
      scroller = TRUE,
      fixedColumns = list(leftColumns = 1, rightColumns = 0)
    )
)

## ----echo = FALSE, warning = FALSE, message = FALSE---------------------------
DT::datatable(
  head(airship::ExampleData1),
    filter = "bottom",
    class = 'cell-border stripe',
    extensions = c('Scroller', 'FixedColumns'),
    options = list(
      dom = 'Bfrtip',
      scrollX = TRUE,
      fixedColumns = TRUE,
      deferRender = TRUE,
      scrollY = 200,
      scroller = TRUE,
      fixedColumns = list(leftColumns = 1, rightColumns = 0)
    )
)

## ----echo = FALSE, warning = FALSE, message = FALSE, fig.width = 7------------
library(ggpubr)
library(dplyr)

g1 <- 
  ggplot2::ggplot(airship::ExampleData1) + 
  ggplot2::geom_boxplot(alpha = 0.1) + 
  ggplot2::aes(x = input1, y = output1)

g2 <- 
  ggplot2::ggplot(
    airship::ExampleData1 |> 
      dplyr::filter(
        input2 == 1,
        input3 == "Z",
        input4 == 11
      )
    ) + 
  ggplot2::geom_boxplot(alpha = 0.1) + 
  ggplot2::aes(x = input1, y = output1)

g3 <- ggpubr::ggarrange(g2, g1, ncol = 2, labels = c("a", "b"))
g3

## ----eval = FALSE-------------------------------------------------------------
#  airship()

## ----eval = FALSE-------------------------------------------------------------
#  airship(
#    dfData = ExampleData1,
#    cLastInputVar = "input4",
#    cReplicationVar = "replications"
#  )

