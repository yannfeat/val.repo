## ----knitr, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----load libraries-----------------------------------------------------------
library(ale)

## ----attitude_str-------------------------------------------------------------
str(attitude)

## ----attitude_summary---------------------------------------------------------
summary(attitude)

## ----lm_summary---------------------------------------------------------------
lm_attitude <- lm(rating ~ ., data = attitude)

summary(lm_attitude)

## ----lm_simple, fig.width=7, fig.height=6-------------------------------------

# # To generate the code, uncomment the following lines.
# # For speed, this vignette loads a pre-created ALE object.
# # For standard models like lm that store their data,
# # there is no need to specify the data argument.
# ale_lm_attitude_simple <- ALE(lm_attitude)
# saveRDS(ale_lm_attitude_simple, file.choose())
ale_lm_attitude_simple <- url('https://github.com/tripartio/ale/raw/main/download/ale_lm_attitude_simple.0.5.2.rds') |> 
  readRDS()

# Print all plots
plot(ale_lm_attitude_simple) |> 
  print(ncol = 2)

## ----lm_full_call-------------------------------------------------------------
# # To generate the code, uncomment the following lines.
# # For speed, this vignette loads a pre-created ModelBoot object.
# # For standard models like lm that store their data,
# # there is no need to specify the data argument.
# mb_lm_attitude <- ModelBoot(lm_attitude) # 100 bootstrap iterations by default
# saveRDS(mb_lm_attitude, file.choose())
mb_lm_attitude <- url('https://github.com/tripartio/ale/raw/main/download/mb_lm_attitude.0.5.2.rds') |> 
  readRDS()


## ----lm_full_stats------------------------------------------------------------
mb_lm_attitude@model_stats

## ----lm_full_coefs------------------------------------------------------------
mb_lm_attitude@model_coefs

## ----lm_full_ale, fig.width=7, fig.height=6-----------------------------------
plot(mb_lm_attitude) |> 
  print(ncol = 2)

## ----gam_summary--------------------------------------------------------------
gam_attitude <- mgcv::gam(
  rating ~ complaints + privileges + s(learning) +
    raises + s(critical) + advance,
  data = attitude)
summary(gam_attitude)

## ----gam_simple, fig.width=7, fig.height=6------------------------------------
# # To generate the code, uncomment the following lines.
# # For speed, this vignette loads a pre-created ALE object.
# # For standard models like gam that store their data,
# # there is no need to specify the data argument.
# ale_gam_attitude_simple <- ALE(gam_attitude)
# saveRDS(ale_gam_attitude_simple, file.choose())
ale_gam_attitude_simple <- url('https://github.com/tripartio/ale/raw/main/download/ale_gam_attitude_simple.0.5.2.rds') |> 
  readRDS()

plot(ale_gam_attitude_simple) |> 
  print(ncol = 2)

## ----gam_full_stats-----------------------------------------------------------
# # To generate the code, uncomment the following lines.
# # For speed, this vignette loads a pre-created ModelBoot object.
# # For standard models like lm that store their data,
# # there is no need to specify the data argument.
# # 100 bootstrap iterations by default.
# mb_gam_attitude <- ModelBoot(gam_attitude)
# saveRDS(mb_gam_attitude, file.choose())
mb_gam_attitude <- url('https://github.com/tripartio/ale/raw/main/download/mb_gam_attitude.0.5.2.rds') |> 
  readRDS()

mb_gam_attitude@model_stats

## ----gam_full_coefs-----------------------------------------------------------
mb_gam_attitude@model_coefs

## ----gam_full_ale, fig.width=7, fig.height=6----------------------------------
plot(mb_gam_attitude) |> 
  print(ncol = 2)

## ----gam_summary_repeat-------------------------------------------------------
gam_attitude_again <- mgcv::gam(
  rating ~ complaints + privileges + s(learning) +
    raises + s(critical) + advance,
  data = attitude)
summary(gam_attitude_again)

## ----model_call_string--------------------------------------------------------

# # To generate the code, uncomment the following lines.
# # For speed, this vignette loads a pre-created ModelBoot object.
# # For standard models like gam that store their data,
# # there is no need to specify the data argument.
# # 100 bootstrap iterations by default.
# mb_gam_attitude_non_standard <- ModelBoot(
#   gam_attitude_again,
#   model_call_string = 'mgcv::gam(
#     rating ~ complaints + privileges + s(learning) +
#       raises + s(critical) + advance,
#     data = boot_data)'
# )
# saveRDS(mb_gam_attitude_non_standard, file.choose())
mb_gam_attitude_non_standard <- url('https://github.com/tripartio/ale/raw/main/download/mb_gam_attitude_non_standard.0.5.2.rds') |> 
  readRDS()

mb_gam_attitude_non_standard@model_stats

