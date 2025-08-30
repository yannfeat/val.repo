## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup---------------------------------------------------------------
library(add2ggplot)

## ------------------------------------------------------------------------
add_logo(
    plot_path = system.file("extdata", "logo.png", package = "add2ggplot"),
    logo_path = system.file("extdata", "jiaxiang.png", package = "add2ggplot"),
    logo_position = "bottom right",
    logo_scale = 5
)

## ------------------------------------------------------------------------
 more_colors(n = 12)
 more_colors(n = 24)
 more_colors(n = 36)

## ------------------------------------------------------------------------
mtcars %>% 
  ggplot2::ggplot(ggplot2::aes(mpg, disp)) +
  ggplot2::geom_point() +
  theme_classic2()

## ------------------------------------------------------------------------
mtcars %>% 
  ggplot2::ggplot(ggplot2::aes(mpg, disp)) +
  ggplot2::geom_point() +
  theme_du_bois()

## ------------------------------------------------------------------------
mtcars %>% 
  ggplot2::ggplot(ggplot2::aes(mpg, disp)) +
  ggplot2::geom_point() +
  theme_grey_and_red()

## ------------------------------------------------------------------------
mtcars %>% 
  ggplot2::ggplot(ggplot2::aes(mpg, disp)) +
  ggplot2::geom_point() +
  theme_ilo()

## ------------------------------------------------------------------------
mtcars %>% 
  ggplot2::ggplot(ggplot2::aes(mpg, disp)) +
  ggplot2::geom_point() +
  theme_white()

