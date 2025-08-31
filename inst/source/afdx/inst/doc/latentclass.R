## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=T,warning=F,message=F----------------------------------------
library(afdx)

## ----echo = F, result = 'markup'----------------------------------------------
library(dplyr)
library(tidyr)
library(magrittr)
library(knitr)
library(kableExtra)

kable(
  head(malaria_df1, n = 6),
       caption = "head(malaria_df1) first 6 observations", 
       format = "html") %>%
kable_styling( position = "left")

## ----echo = F-----------------------------------------------------------------
cutoffs <- c(0,1,100,200,400,800,1600,3200,6400,12800,25600,51200, 102400, 204800)
data <- 
  malaria_df1 %>%
  mutate(k = cut(density,c(cutoffs,Inf), include.lowest =T, labels = cutoffs)) %>%
  group_by(k,fever) %>%
  tally() %>%
  mutate(category = ifelse(fever ==1,"n (fever)","m (no fever)")) %>%
  select(-fever) %>%
  pivot_wider(names_from = "category", values_from = "n", values_fill = list(n = 0)) %>%
  rename(`k (category lower limit)` = k)

kable(data, "html", caption="Distribution of fevers by density categories") %>%
kable_styling(position = "left")  

## -----------------------------------------------------------------------------
model <- get_latent_model()
cat(model)

## ---- eval=FALSE--------------------------------------------------------------
#  library(rjags)
#  library(coda)
#  
#  # compile the model
#  af_latent <-
#    jags.model(
#      textConnection(get_latent_model()),
#      data = list(n = data$`n (fever)`,
#                  m = data$`m (no fever)`),
#      n.chains = 4,
#      n.adapt = 1000,
#      inits = list(
#        list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 1111),
#        list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 2222),
#        list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 3333),
#        list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 4444)
#      )
#    )
#  
#  # Simulate the posterior
#  latent_sim <-
#    coda.samples(
#      model = af_latent,
#      variable.names = c('lambda','sens','spec','ppv','npv'),
#      n.thinning = 5,
#      n.iter =  10000 )
#  
#  # Extract and Analyze the posterior
#  latent_sum <-  summary(latent_sim)
#  latent_eff <-  effectiveSize(latent_sim)

## ---- echo=FALSE, include=FALSE-----------------------------------------------
# Load results from the model
library(coda)
latent_sum <- readRDS(system.file("vignette_data/latent_sum.RDS", package = "afdx"))
latent_eff <- readRDS(system.file("vignette_data/latent_eff.RDS", package = "afdx"))

## -----------------------------------------------------------------------------
# reformat to present the results
summary_table <-
  data.frame(latent_sum[[1]]) %>%
  bind_cols(data.frame(latent_sum[[2]])) %>%
  mutate(varname = row.names(latent_sum[[1]])) %>%
  mutate(cutoff  = c(NA, rep(cutoffs,4))) %>%
  select(varname, cutoff,Mean, X2.5., X50., X97.5.,Naive.SE ) %>%
  mutate(eff_size = floor(latent_eff)) %>%
  filter(is.na(cutoff) | cutoff != 0) 


mean_table <- summary_table %>%
  rename(point = Mean) %>%
  rename(lci = `X2.5.`) %>%
  rename(uci = `X97.5.`) %>%
  mutate(varname = gsub("\\[.*\\]","",varname)) %>%
  filter(varname != "lambda") %>%
  select(cutoff,varname, lci, uci,point) %>%
  pivot_longer(-c("cutoff","varname"),names_to = "xxv", values_to = "value") %>%
  unite("varx",varname,xxv ) %>%
  pivot_wider(names_from = "varx", values_from = "value") %>%
  select(cutoff,
         sens_point, 
         sens_lci, 
         sens_uci, 
         spec_point,
         spec_lci, 
         spec_uci,
         ppv_point, 
         ppv_lci, 
         ppv_uci,
         npv_point,
         npv_lci,
         npv_uci) %>%
  rename(sensitivity = sens_point) %>%
  rename(specificity = spec_point) %>%
  rename(ppv = ppv_point) %>%
  rename(npv = npv_point) %>%
  mutate_if(is.numeric, round,3)

# Lambda corresponds to the attributable fraction
afrow <- 
  summary_table %>%
  filter(varname == "lambda") %>% 
  mutate_if(is.numeric, round,3)

## ---- echo = F, result = 'markup', out.width= "90%"---------------------------
kable(mean_table, caption = "Summary of diagnostic characteristics at selected cutoff points") %>% kable_styling()

