## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(afdx)

## ----echo = F, result = 'markup'----------------------------------------------
library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(knitr)
library(kableExtra)

kable(
  head(malaria_df1, n = 6),
       caption = "head(malaria_df1) first 6 observations", 
       format = "html") %>%
kable_styling( position = "left")

## ----echo = F, result = 'asis'------------------------------------------------

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

## ----echo =FALSE--------------------------------------------------------------
fit <- fit <- logitexp(malaria_df1$fever, malaria_df1$density)
cutoff = 500
xx <- senspec(fit,cutoff)
N = sum(malaria_df1$fever)
nc = sum(malaria_df1$density > cutoff & malaria_df1$fever)
L = fit$af
lc = xx[1,"ppv"]
df <- data.frame(
  "True Malaria" = c(nc*lc, N*L-nc*lc, N*L),
  "Other aetiology" = c(nc*(1-lc), N*(1-L)- nc*(1-lc), N-nc),
  "All" = c(nc, N-nc, N)
)
row.names(df)<- c("Malaria case (fever and density > 500)", "No case", "Total")
df <- round(df,1)


## ----results='markdown', echo = F---------------------------------------------
kable(df, "html", caption = "Case definition with 500 as cutoff") %>% kable_styling(position = "left")

## -----------------------------------------------------------------------------

fit <- logitexp(malaria_df1$fever, malaria_df1$density)
fit

senspec(fit, c(1,100,500,1000,2000,4000,8000,16000, 32000,54000,100000))

## ---- fig.width = 8, fig.height=6, out.width= "100%", dpi = 300---------------
cutoffs <-  make_cutoffs(malaria_df1$fever, malaria_df1$density)
dxp <- senspec(fit, cutoffs[-1])
dxp %>%
  data.frame(.) %>%
  pivot_longer(-cutoff, names_to = "Indicator",values_to = "Value") %>%
  ggplot(aes(x = cutoff, y = Value, color = Indicator, linetype = Indicator)) +
  geom_line() +
  scale_x_log10("Cutoff")

## ---- fig.width = 8, fig.height=6, out.width= "100%", dpi = 300---------------
rocdf <-dxp %>%
  data.frame(.) %>%
  ## add the corners
  bind_rows(
    data.frame(
      sensitivity= c(1,0),
      specificity= c(0,1)
    )
  ) %>%
  # generate the 1-specificity
  mutate(`1-specificity` = 1 - specificity) 

  # make the graph
  ggplot(rocdf, aes(x = `1-specificity`, y = sensitivity)) +
  geom_line()+
  ggtitle("ROC curve")
  
  # Estimate the area under the curve
  library(DescTools)
  AUC(rocdf$`1-specificity`, rocdf$sensitivity)
  

