## ---- eval=TRUE, echo=FALSE---------------------------------------------------
library(adnuts)

## -----------------------------------------------------------------------------
fit <- readRDS('fit.RDS')
print(fit)
summary(fit$monitor$n_eff)
summary(fit$monitor$Rhat)

## -----------------------------------------------------------------------------
post <- extract_samples(fit)
str(post[,1:5])
sp <- extract_sampler_params(fit)
str(sp)

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  post <- extract_samples(fit, as.list=TRUE)
#  postlist <- coda::mcmc.list(lapply(post, coda::mcmc))
#  coda::traceplot(postlist)

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  library(bayesplot)
#  library(dplyr)
#  library(tidyr)
#  library(ggplot2)
#  color_scheme_set("red")
#  np <- extract_sampler_params(fit) %>%
#    pivot_longer(-c(chain, iteration), names_to='Parameter', values_to='Value') %>%
#    select(Iteration=iteration, Parameter, Value, Chain=chain) %>%
#    mutate(Parameter=factor(Parameter),
#           Iteration=as.integer(Iteration),
#           Chain=as.integer(Chain)) %>% as.data.frame()
#  mcmc_nuts_energy(np) + ggtitle("NUTS Energy Diagnostic") + theme_minimal()

## ----fig1, fig.width=6, fig.height=4.5----------------------------------------
plot_marginals(fit, pars=1:9)

## ----fig2, fig.width=6, fig.height=4.5----------------------------------------
pairs_admb(fit, pars=1:3, order='slow')
pairs_admb(fit, pars=c('sigmaphi', 'sigmap', 'sigmayearphi'))

