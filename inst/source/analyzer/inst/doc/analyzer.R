## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load_libs, warning = FALSE, message = FALSE------------------------------
library(analyzer)
library(ggplot2)

## -----------------------------------------------------------------------------
# For a continuous vector
explainer(mtcars$mpg, quant.seq = c(0, 0.25, 0.5, 1),
          include.numeric = c("trimmed.means", "skewness", "kurtosis"))

## -----------------------------------------------------------------------------
# For a categorical vector
explainer(as.factor(mtcars$cyl))

## -----------------------------------------------------------------------------
# For a complete data.frame
explainer(mtcars)

## ----plot_var, warning = FALSE, message = FALSE-------------------------------
# Simple plot for one variable
p <- plottr(mtcars$mpg)
plot(p$x)

## ----plot_df, warning = FALSE, message = FALSE--------------------------------
# default plots for all the variables in mtcars
p <- plottr(mtcars, yvar = "disp", yclass = "numeric")
plot(p$mpg)

## ----plot_custom, warning = FALSE---------------------------------------------
# Define a function for plot for continuous independent and Continuous dependent variables
custom_plot_for_continuous_vars <- function(dat, xname, yname, ...) {
  
  xyplot <- ggplot(dat, aes_string(x=xname, y=yname)) +
    geom_point(alpha = 0.6, color = "#3c4fde") + geom_rug() +
    theme_minimal()
  xyplot <- gridExtra::arrangeGrob(xyplot,
                                   top = paste0("New plot of ",
                                                xname, " and ", yname)
  )

  return(xyplot)
  
}

## ----plot_df2, warning = FALSE, message = FALSE-------------------------------
p <- plottr(mtcars, yvar = "disp", yclass = "numeric", 
            FUN3 = custom_plot_for_continuous_vars)
plot(p$mpg)

## ----plot_custom2, warning = FALSE--------------------------------------------
# Define a function for plot for continuous independent and Continuous dependent variables
custom_plot2 <- function(dat, xname, ...) {
  
  # histogram
  p1 <- ggplot(dat, aes_string(x=xname)) +
    geom_histogram(fill="#77bf85", color = "black") +
    theme_minimal()

  return(p1)
  
}

## ----plot_df3, warning = FALSE, message = FALSE-------------------------------
p <- plottr(mtcars, yvar = "disp", yclass = "numeric",
            FUN1 = custom_plot2, 
            FUN3 = custom_plot_for_continuous_vars)
plot(p$mpg)

## ----association, warning = FALSE---------------------------------------------
corr_all <- association(mtcars, categorical = c('cyl', 'vs', 'am', 'gear'), normality_test_method = 'ks')

## ----sel_methods--------------------------------------------------------------
corr_all$method_used

## -----------------------------------------------------------------------------
corr_all$method_used

## ----norm_test----------------------------------------------------------------
norm_test_fun(mtcars$mpg, method = "anderson")

