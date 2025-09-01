## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----load libraries-----------------------------------------------------------
library(ale)
library(dplyr)

## ----diamonds_print-----------------------------------------------------------
# Clean up some invalid entries
diamonds <- ggplot2::diamonds |> 
  filter(!(x == 0 | y == 0 | z == 0)) |> 
  # https://lorentzen.ch/index.php/2021/04/16/a-curious-fact-on-the-diamonds-dataset/
  distinct(
    price, carat, cut, color, clarity,
    .keep_all = TRUE
  ) |> 
  rename(
    x_length = x,
    y_width = y,
    z_depth = z,
    depth_pct = depth
  )

summary(diamonds)

## ----diamonds_str-------------------------------------------------------------
str(diamonds)

## ----diamonds_price-----------------------------------------------------------
summary(diamonds$price)

## ----train_gam----------------------------------------------------------------
# Create a GAM model with flexible curves to predict diamond prices.
# Smooth all numeric variables and include all other variables.
gam_diamonds <- mgcv::gam(
  price ~ s(carat) + s(depth_pct) + s(table) + s(x_length) + s(y_width) + s(z_depth) +
    cut + color + clarity,
  data = diamonds
  )
summary(gam_diamonds)

## ----ale_simple---------------------------------------------------------------
# Simple ALE without bootstrapping

# # To generate the code, uncomment the following lines.
# # For speed, this vignette loads a pre-created ALE object.
# # For standard models like mgcv::gam that store their data,
# # there is no need to specify the data argument.
# ale_gam_diamonds <- ALE(gam_diamonds)
# saveRDS(ale_gam_diamonds, file.choose())
ale_gam_diamonds <- url('https://github.com/tripartio/ale/raw/main/download/ale_gam_diamonds.0.5.2.rds') |> 
  readRDS()

## ----create-plots-------------------------------------------------------------
# Print a plot by entering its reference
diamonds_plots <- plot(ale_gam_diamonds)

## ----print-carat, fig.width=3.5, fig.width=4----------------------------------
# Print a plot by entering its reference
get(diamonds_plots, 'carat')

## ----print-ale_simple, fig.width=7, fig.height=11-----------------------------
# Print all plots
plot(diamonds_plots, ncol = 2)

## ----ale_boot, fig.width=7, fig.height=11-------------------------------------

# # To generate the code, uncomment the following lines.
# # For speed, this vignette loads a pre-created ALE object.
# # For standard models like mgcv::gam that store their data,
# # there is no need to specify the data argument.
# ale_gam_diamonds_boot <- ALE(
#   gam_diamonds, 
#   boot_it = 100
# )
# saveRDS(ale_gam_diamonds_boot, file.choose())
ale_gam_diamonds_boot <- url('https://github.com/tripartio/ale/raw/main/download/ale_gam_diamonds_boot.0.5.2.rds') |> 
  readRDS()

# Bootstrapping produces confidence intervals
plot(ale_gam_diamonds_boot) |> 
  print(ncol = 2)

## ----ale_2D-------------------------------------------------------------------
# ALE two-way interactions

# # To generate the code, uncomment the following lines.
# # For speed, this vignette loads a pre-created ALE object.
# # For standard models like mgcv::gam that store their data,
# # there is no need to specify the data argument.
# ale_2D_gam_diamonds <- ALE(
#   gam_diamonds,
#   x_cols = list(d2 = TRUE)
# )
# saveRDS(ale_2D_gam_diamonds, file.choose())
ale_2D_gam_diamonds <- url('https://github.com/tripartio/ale/raw/main/download/ale_2D_gam_diamonds.0.5.2.rds') |> 
  readRDS()


## ----print-all-2D, fig.width=7, fig.height=7----------------------------------
diamonds_2D_plots <- plot(ale_2D_gam_diamonds)

diamonds_2D_plots |>
  # Select all 2D interactions that involve 'carat'
  subset(list(d2_all = 'carat')) |> 
  print(ncol = 2)

## ----print-specific-ixn, fig.width=5, fig.height=3----------------------------
get(diamonds_2D_plots, ~ carat:clarity)

