## ----include = FALSE----------------------------------------------------------
  knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  )

## ----setup--------------------------------------------------------------------
library(alien)

## ----theme setup, echo = FALSE------------------------------------------------
  ggplot2::theme_set(
  ggplot2::theme_bw()+
  ggplot2::theme(axis.title = ggplot2::element_text(size = 20),
  axis.text = ggplot2::element_text(size = 18),
  panel.grid = ggplot2::element_blank())
  )

## -----------------------------------------------------------------------------
data("sfestuary")
print(sfestuary)

## ----plotting_data, fig.width = 8, fig.height= 4.5, fig.align='center'--------
library(alien)
library(ggplot2)

years <- seq_along(sfestuary) + 1850 # set starting year for the figure

ggplot()+
  aes(x = years, y = cumsum(sfestuary))+
  geom_line() + 
  coord_cartesian(ylim = c(0,150))+
  scale_x_continuous(breaks = seq(1860, 1980, 20)) + 
  scale_y_continuous(breaks = seq(0, 150, 50)) + 
  ylab("Cumulative discoveries") + theme(axis.title.x = element_blank())


## ----fitting model------------------------------------------------------------
model <- snc(y = sfestuary, control = list(maxit = 1e4))

## ----printing model-----------------------------------------------------------
names(model)

## -----------------------------------------------------------------------------
model$records

## -----------------------------------------------------------------------------
model$convergence

## -----------------------------------------------------------------------------
model$`log-likelihood`

## -----------------------------------------------------------------------------
model$coefficients

## -----------------------------------------------------------------------------
head(model$predict, 4) 

## ----plotting_fit, fig.width = 8, fig.height= 4.5, fig.align='center'---------

plot_snc(model, cumulative = T) +
  coord_cartesian(ylim = c(0,150))+
  scale_y_continuous(breaks = seq(0, 150, 50)) + 
  ylab("Cumulative discoveries") + 
  xlab("Years since first record in data")

## -----------------------------------------------------------------------------
constant_detection <- snc(sfestuary, pi = ~ 1, growth = FALSE)

## -----------------------------------------------------------------------------
constant_detection$`log-likelihood`

## -----------------------------------------------------------------------------
constant_introduction <- snc(sfestuary, mu = ~1)

## -----------------------------------------------------------------------------
constant_introduction$`log-likelihood`

## ----medfish_data-------------------------------------------------------------
data("medfish")
head(medfish)

## ----medfish_plot, fig.width = 8, fig.height= 4.5, fig.align='center'---------
ggplot2::ggplot(medfish)+
  ggplot2::aes(x = year) + 
  ggplot2::geom_point(ggplot2::aes(y = cumsum(natives)), shape = 21, size = 2, fill = "#377EB8") +
  ggplot2::geom_point(ggplot2::aes(y = cumsum(aliens)), shape = 21,  size = 2, fill = "#E41A1C")

## -----------------------------------------------------------------------------
medfish_for_model <- dplyr::mutate(medfish, natives_scaled = scale(natives))

## -----------------------------------------------------------------------------
sampling_proxy_model <- snc(aliens, pi = ~ natives_scaled, data = medfish_for_model, control = list(maxit = 1000))

