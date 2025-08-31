## ---- include = FALSE, echo=FALSE, message=FALSE------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width=5,
  fig.height=5
)

library(dplyr)
library(ggplot2)
library(ags)

## ---- include = FALSE, echo=FALSE, warning = FALSE, message = FALSE-----------
d <- ags:::xd19 %>% 
  group_by(year_xw,ags19) %>% 
  summarize(n=mean(size_conv)<1) %>% 
  group_by(year_xw) %>% 
  summarize(p=(sum(n)/n())*100) %>% 
  mutate(type="Districts")

m <- ags:::xm19 %>% 
  group_by(year_xw,ags19) %>% 
  summarize(n=mean(size_conv)<1) %>% 
  group_by(year_xw) %>% 
  summarize(p=(sum(n)/n())*100) %>% 
  mutate(type="Municipalities")

df <- bind_rows(d,m) %>% 
  rename(Year=year_xw) %>% 
  filter(Year!=2019)

## ---- echo=FALSE, warning = FALSE, message = FALSE----------------------------
ggplot(df, aes(Year,p,color=type,group=type)) + 
  geom_line() + ylab("Share (in %)") + 
  theme(legend.position = "bottom") + 
  scale_color_brewer(palette="Set1") + 
  labs(color='', subtitle="% Districts and municipalities requiring spatial interpolation")

## -----------------------------------------------------------------------------
data(btw_sn)
ggplot(btw_sn, aes(year, (valid/voters)*(100), group=district)) + 
geom_line() + geom_point() + ylab("Turnout (in %)") + xlab("Year")

## -----------------------------------------------------------------------------
 btw_sn_ags20 <- xwalk_ags(
  data=btw_sn, 
  ags="district", 
  time="year", 
  xwalk="xd20", 
  variables=c("voters", "valid"), 
  weight="pop")

## -----------------------------------------------------------------------------
 btw_sn_ags20 <- xwalk_ags(
  data=btw_sn, 
  ags="district", 
  time="year", 
  xwalk="xd20", 
  variables=c("voters", "valid"), 
  fuzzy_time=TRUE,
  weight="pop")

## -----------------------------------------------------------------------------
 ggplot(btw_sn_ags20, aes(year, (valid/voters)*100, group=ags20)) + 
 geom_line() + geom_point() + ylab("Turnout (in %)") + xlab("Year") 

