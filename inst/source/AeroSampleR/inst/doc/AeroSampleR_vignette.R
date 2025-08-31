## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
library(AeroSampleR)
library(ggplot2)
library(dplyr)
library(flextable)

## ----echo=FALSE, message=FALSE, warning=FALSE, comment = ">"------------------
sys_df <- structure(list( el_num = c("1", "2", "3", "4"), 
    el_type = c("probe", "tube", "bend", "tube"), 
    length_cm = c(NA, 111.76, NA, 146.05), 
    angle_to_horiz = c(NA, 90, NA, 0), 
    orient = c("u", NA, NA, NA), 
    bend_angle = c(NA, NA, 90, NA), 
    bend_rad_cm = c(NA, NA, 12.7, NA)), 
    row.names = c(NA, -4L), 
    class = c("tbl_df", "tbl", "data.frame"))
cat("\n")

## ----echo=FALSE, message=FALSE, warning=FALSE, comment = ">"------------------
ft <- flextable(sys_df)
ft <- colformat_double(ft, digits = 0)
ft

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  sys_df <- read.table(
#    file = "c:/work/system.txt",
#    header = TRUE
#    )

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  sys_df <- readxl::read_xlsx(path = "c:/work/system.xlsx",
#             sheet = "Sheet1", #default - update if needed
#             range = "A1:G5", #put in entire range
#             col_types = c("numeric",
#                           "text",
#                           "numeric",
#                           "numeric",
#                           "text",
#                           "numeric",
#                           "numeric")
#                              )

## ----echo=TRUE, message=TRUE, warning=FALSE-----------------------------------
df <- particle_dist() #Default


## ----echo=FALSE, message=TRUE, warning=FALSE, fig.width=5, fig.height= 3------
df |> filter(dist == "log_norm") |> 
  ggplot(aes(D_p, dens)) + geom_point(color = "blue") +
  ggtitle("distribution of lognormal particle sizes")

df |> filter(dist == "log_norm") |> 
  mutate("activity" = D_p ^3 * dens)  |> 
  ggplot(aes(D_p, activity)) + geom_point(color = "blue") +
  ggtitle("relative activity by particle size",
          subtitle = "diameter cubed times density")


## ----echo=TRUE, message=TRUE, warning=FALSE-----------------------------------
# In this example the tubing wall is 1.65 mm thick. 
params <- set_params_1("D_tube" = 2.54 - (2 * 0.165), #1 inch tube diameter
                       "Q_lpm" = 2 * 28.3, #2 cfm converted to lpm
                       "T_C" = 25, 
                       "P_kPa" = 101.325)

## ----echo=TRUE, message=TRUE, warning=FALSE-----------------------------------
df <- set_params_2(df, params)

## ----echo=TRUE, message=TRUE, warning=FALSE-----------------------------------
df <- probe_eff(df, params, orient = sys_df$orient[1]) 

## ----echo=TRUE, message=TRUE, warning=FALSE-----------------------------------
df <- tube_eff(df, 
               params, 
               L = sys_df$length_cm[2] / 100, 
               angle_to_horiz = sys_df$angle_to_horiz[2], 
               elnum = sys_df$el_num[2])

## ----echo=TRUE, message=TRUE, warning=FALSE-----------------------------------
df <- bend_eff(df, params, method = "Zhang", 
               bend_angle = sys_df$bend_angle[3],
               bend_radius = sys_df$bend_rad_cm[3] / 100, 
               elnum = sys_df$el_num[3])

## ----echo=TRUE, message=TRUE, warning=FALSE-----------------------------------
df <- tube_eff(df, params, L = sys_df$length_cm[2] / 100, 
               angle_to_horiz = sys_df$angle_to_horiz[4], 
               elnum = sys_df$el_num[4])

## ----echo=TRUE, message=TRUE, warning=FALSE-----------------------------------
tail(df)

## ----echo=TRUE, message=FALSE, warning=FALSE, fig.width=5, fig.height= 3, fig.align = 'center'----
params[, 7] <- formatC(params[, 7], digits = 2, format = "e")
params[, 8] <- formatC(params[, 8], digits = 2, format = "e")
params[, 11] <- formatC(params[, 11], digits = 2, format = "e")
params[, 3] <- formatC(params[, 3], digits = 4)
params[, 10] <- formatC(params[, 10], digits = 4)
ft <- flextable(params)
ft <- set_caption(ft, "system parameters")
ft

## ----echo=TRUE, message=FALSE, warning=FALSE, fig.width=5, fig.height= 3, fig.align = 'center'----

ft <- flextable(report_basic(df, params, "discrete"))
ft <- colformat_double(ft, digits = 3)
ft <- set_caption(ft, "results for discrete particle diameters")
ft

report_plots(df, "discrete")
report_cum_plots(df, 1)
report_cum_plots(df, 5)
report_cum_plots(df, 10)


ft <- flextable(report_basic(df, params, "log"))
ft <- colformat_double(ft, digits = 3)
ft <- set_caption(ft, "results for log distribution of particle diameters")
ft  


## ----echo=TRUE, message=TRUE, warning=FALSE, fig.width=5, fig.height= 3, fig.align = 'center'----
df_log <- report_log_mass(df)[sort(sample(1:1000, 10)), ]
# need to make format changes so that flextable will show scientific notation
df_log[, 1] <- formatC(df_log[, 1], digits = 4)
df_log[, 2] <- formatC(df_log[, 2], digits = 2, format = "e")
df_log[, 3] <- formatC(df_log[, 3], digits = 2, format = "e")
df_log[, 4] <- formatC(df_log[, 4], digits = 2, format = "e")
df_log[, 5] <- formatC(df_log[, 5], digits = 2, format = "e")
df_log[, 6] <- formatC(df_log[, 6], digits = 2, format = "e")
df_log[, 7] <- formatC(df_log[, 7], digits = 2, format = "e")
ft <- flextable(df_log)
ft <- colformat_double(ft, digits = 3)
ft <- set_caption(ft, "results for random sample of 1000 particle diameters from the log set")
ft

## ----echo=TRUE, message=TRUE, warning=FALSE, fig.width=5, fig.height= 3, fig.align = 'center'----
report_plots(df, "log")

