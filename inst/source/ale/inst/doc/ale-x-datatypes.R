## ----knitr, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load libraries-----------------------------------------------------------
library(ale)
library(dplyr)

## ----print var_cars-----------------------------------------------------------
print(var_cars)

## ----var_cars summary---------------------------------------------------------
summary(var_cars)

## ----gam_cars-----------------------------------------------------------------
gam_cars <- mgcv::gam(
  mpg ~ cyl + disp + hp + drat + wt + s(qsec) +
    vs + am + gear + carb + country,
  data = var_cars
)
summary(gam_cars)

## ----ale_cars_1D, fig.width=7, fig.height=14----------------------------------
# # To generate the code, uncomment the following lines.
# # For speed, this vignette loads a pre-created ALE object.
# # For standard models like gam that store their data,
# # there is no need to specify the data argument.
# ale_cars_1D <- ALE(gam_cars)
# saveRDS(ale_cars_1D, file.choose())
ale_cars_1D <- url('https://github.com/tripartio/ale/raw/main/download/ale_cars_1D.0.5.2.rds') |> 
  readRDS()


# Print all plots
plot(ale_cars_1D) |> 
  print(ncol = 2)

## ----ale_cars_2D, fig.width=7, fig.height=28----------------------------------
# # To generate the code, uncomment the following lines.
# # For speed, this vignette loads a pre-created ALE object.
# # For standard models like gam that store their data,
# # there is no need to specify the data argument.
# ale_cars_2D <- ALE(
#   gam_cars,
#   x_cols = list(d2 = TRUE)
# )
# saveRDS(ale_cars_2D, file.choose())
ale_cars_2D <- url('https://github.com/tripartio/ale/raw/main/download/ale_cars_2D.0.5.2.rds') |> 
  readRDS()

# Print plots
plot(ale_cars_2D) |> 
  print(
    ncol = 2, 
    # By default, at most 20 plots are printed. Set max_print to increase this limit
    max_print = 100
  )

## ----cars_full, fig.width=7, fig.height=14------------------------------------
# # To generate the code, uncomment the following lines.
# # For speed, this vignette loads a pre-created ModelBoot object.
# # For standard models like lm that store their data,
# # there is no need to specify the data argument.
# # 100 bootstrap iterations by default.
# mb_cars <- ModelBoot(gam_cars)
# saveRDS(mb_cars, file.choose())
mb_cars <- url('https://github.com/tripartio/ale/raw/main/download/mb_cars.0.5.2.rds') |> 
  readRDS()

plot(mb_cars) |> 
  print(ncol = 2)

