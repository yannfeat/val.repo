## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.height = 6, fig.width = 6)

## ---- message=FALSE-----------------------------------------------------------
#=======================================================================
# Packges
#=======================================================================
#devtools::install_github("AlphaGenes/AlphaPart")
library(AlphaPart)
library(dplyr)
library(ggplot2)
library(ggridges)

## -----------------------------------------------------------------------------
#=======================================================================
# Reading and organizing Scenario 1
#=======================================================================
data <-  readRDS("./../inst/extdata/AlphaPartCattleSim.rds") %>%
  dplyr::mutate(across(generation:mother, as.numeric)) %>%
  dplyr::rename(status = type) %>%
  dplyr::mutate(across(c("sex", "status"), as.factor)) %>%
  dplyr::mutate(path = interaction(sex,status, sep = ":")) %>%
  arrange(generation, ind) %>%
  select(ind, father, mother, sex, status, path, generation, tbv, pheno) %>%
  dplyr::mutate(generation = generation - 20) %>%
  droplevels()

# Data head
head(data) %>%
  knitr::kable(digits = 2)

# Data size
dim(data)

## -----------------------------------------------------------------------------
part <- AlphaPart(data, colId = "ind", colFid = "father", 
                  colMid = "mother", colBV = "tbv", colPath = "path")
head(part$tbv) %>%
  knitr::kable(digits = 2)

## -----------------------------------------------------------------------------
# Trends in the additve genetic mean
partMean <- summary(part, by = "generation", FUN = mean)

head(partMean$tbv) %>%
  knitr::kable(digits = 2)

## -----------------------------------------------------------------------------
# Trends in the additive genetic variance
partVar <- summary(part, by = "generation", FUN = var, cov = TRUE)

head(partVar$tbv) %>%
  knitr::kable(digits = 2)

## ---- fig.height = 10, fig.width=9--------------------------------------------
part$tbv %>%
  ggplot(aes(y = as.factor(generation), `tbv_F:Non-Selected`)) +
  geom_density_ridges(
    aes(fill = "F - Non-Selected", linetype = "F - Non-Selected"),
    alpha = .4, point_alpha = 1, rel_min_height = 0.01
  ) +
  geom_density_ridges(
    aes(y = as.factor(generation), x= `tbv_M:Non-Selected`, fill = "M - Non-Selected",
        linetype = "M - Non-Selected"),
    alpha = .4, point_alpha = 1, rel_min_height = 0.01
  ) +
  geom_density_ridges(
    aes(y = as.factor(generation), x= `tbv_M:Selected`, fill = "M - Selected",
        linetype = "M - Selected"),
    alpha = .4, point_alpha = 1, rel_min_height = 0.01
  ) +
  geom_density_ridges(
    aes(y = as.factor(generation), x= `tbv`,
        fill = "Sum", linetype = "Sum"),
    alpha = .4, point_alpha = 1, rel_min_height = 0.01
  ) +
  ylab("Generation") +
  xlab("Density plot of breeding value partitions") +
  labs(fill = "Path:", linetype = "Path:") +
  theme_bw(base_size = 20) +
  theme(
    legend.position = "top"
  ) 

## -----------------------------------------------------------------------------
partMean$tbv %>%
  ggplot(aes(y = Sum, x = generation, colour = "Sum"),
         size = 0.1) +
  scale_linetype_manual(
    values = c("solid", "longdash", "dashed", "dotted"))+
  geom_line() +
  geom_line(aes(y = `F:Non-Selected`, x = generation, 
                colour = "F"), alpha = 0.8) +
  geom_line(aes(y = `M:Selected`, x = generation,
                colour = "M(S)"), alpha = 0.8) +
  geom_line(aes(y = `M:Non-Selected`, x = generation,
                colour = "M(N)"), alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = 2, alpha = 0.3) +
  ylab("Genetic Mean") +
  xlab("Generation") +
  labs(colour = "Path:") +
  theme_bw(base_size = 18) + 
  theme(legend.position = "top")


## -----------------------------------------------------------------------------
partVar$tbv %>%
  ggplot(aes(y = Sum, x = generation, colour = "Sum")) +
  geom_line() +
  geom_line(aes(y = `F:Non-Selected`, x = generation,
            colour = "F"), alpha = 0.8) +
  geom_line(aes(y = `F:Non-SelectedM:Selected`, x = generation,
            colour = "F:M(S)"), size =0.5, alpha =0.8) +
  geom_line(aes(y = `F:Non-SelectedM:Non-Selected`, x = generation,
            colour = "F:M(N)"), size =0.5, alpha =0.6) +
  geom_line(aes(y = `M:Non-SelectedM:Selected`, x = generation,
            colour = "M(N):M(S)"), size =0.5, alpha =0.6) +
  geom_line(aes(y = `M:Selected`, x = generation,
            colour = "M(S)"), alpha = 0.8) +
  geom_line(aes(y = `M:Non-Selected`, x = generation,
            colour = "M(N)"), size =0.5, alpha =0.8) +
  geom_vline(xintercept = 0, linetype = 2, alpha = 0.3) +
  ylab("Genetic Variance") +
  xlab("Generation") +
  labs(colour = "Path: ") +
  theme_bw(base_size = 18) +
  theme(
    legend.position = "top"
  )

