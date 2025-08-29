## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(message = FALSE,warning = FALSE)
library(absorber)
library(sparsegl)
library(fda)
library(irlba)
library(ggplot2)

## ----echo = TRUE, eval = FALSE------------------------------------------------
#  install.packages("absorber")

## -----------------------------------------------------------------------------
true.dimensions = c(3,5) ; false.dimensions = c(1,2,4)

## -----------------------------------------------------------------------------
# --- Loading the values of the observation sets --- ##
data('x_obs') ;
head(x_obs)
## --- Loading the values of  corresponding noisy values of the response variable --- ##
data('y_obs') ;
head(y_obs)

## ----Bsplines5D---------------------------------------------------------------
res = absorber(x = x_obs, y = y_obs, M = 3)

## -----------------------------------------------------------------------------
head(res$lambdas)

## -----------------------------------------------------------------------------
head(res$selec.var)

## -----------------------------------------------------------------------------
res$aic.var

## ----plotAbsorber, out.width="50%", fig.align = 'center'----------------------
plot_selection(res)

## -----------------------------------------------------------------------------
nlam = length(res$lambdas)
occurrence = data.frame(table(unlist(res$selec.var))) ; 
colnames(occurrence) = c("Covariable", "Percentage") ;
occurrence$Percentage =occurrence$Percentage*100/nlam ;
occurrence = occurrence[order(-occurrence$Percentage),,drop=FALSE] ;
occurrence$Covariable = factor(occurrence$Covariable,
                                       levels = unique(occurrence$Covariable)) ;

occurrence$Category = as.factor(ifelse(occurrence$Covariable %in% true.dimensions, 
                                   'real features', 'fake features')) ;
str(occurrence) ;

## ----out.width="50%", fig.align = 'center'------------------------------------
color.order = c('firebrick', 'forestgreen')[which( c('fake features', 'real features') 
                                                   %in% levels(occurrence$Category))]

plt_occ = ggplot(data = occurrence, aes(x = Covariable, y = Percentage, fill = Category)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = color.order) +
  ylab('Percentage of selection') +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = 16, face = 'bold'),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15),
        legend.text =  element_text(size = 14),
        legend.position = 'bottom',
        legend.key.size = unit(1, "cm"), 
        panel.grid.major = element_line(size = 0.6, linetype = 'solid',
                                           colour = "darkgrey"), 
           panel.grid.minor = element_line(size = 0.2, linetype = 'solid',
                                           colour = "darkgrey"))

print(plt_occ)

