## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = FALSE------------------------------------------------------------
library(htmltools)
htmltools::img(src = knitr::image_uri("hex-actLifer2.png"), alt = 'logo', style = 'position:absolute; top:0; right:0; padding:10px; width: 100px; height: 100px; border:0')

## ----setup--------------------------------------------------------------------
library(actLifer)

## -----------------------------------------------------------------------------
example <- lifetable(mortality2, "age_group", "population", "deaths")

## ---- echo = FALSE------------------------------------------------------------
example[1:5,]


## ---- echo = FALSE------------------------------------------------------------
mortality2[1:5,]

## -----------------------------------------------------------------------------
lifetable(mortality3, "age_group", "population", "deaths", FALSE, FALSE, FALSE, "gender")

## -----------------------------------------------------------------------------
lifetable(mortality2, "age_group", "population", "deaths", TRUE, FALSE, FALSE)

## -----------------------------------------------------------------------------
mort<- mortality2 %>% 
  central_death_rate("age_group", "population", "deaths") %>% 
  conditional_death_prob("age_group", "population", "deaths")

head(mort)
tail(mort)

