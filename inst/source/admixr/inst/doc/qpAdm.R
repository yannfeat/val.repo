## ----setup, include = FALSE---------------------------------------------------
evaluate <- .Platform$OS.type == "unix" && system("which qpDstat", ignore.stdout = TRUE) == 0 && Sys.getenv("TRAVIS_R_VERSION") == ""

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = evaluate
)

## ---- message = FALSE, warning = FALSE, results = "hide"----------------------
library(admixr)

snps <- eigenstrat(download_data(dirname = tempdir()))

## -----------------------------------------------------------------------------
read_ind(snps)

## -----------------------------------------------------------------------------
models <- qpAdm_rotation(
    data = snps,
    target = "French",
    candidates = c("Dinka", "Mbuti", "Yoruba", "Vindija", "Altai", "Denisova", "Chimp"),
    minimize = TRUE,
    nsources = 2,
    ncores = 2,
    fulloutput = TRUE
)

## -----------------------------------------------------------------------------
models

## ---- warning = FALSE, message = FALSE, fig.width = 6, fig.height = 4---------
library(tidyverse)

select(models$proportions, model, pvalue, prop1, prop2) %>%
    gather(parameter, value, -model) %>%
    ggplot(aes(parameter, value)) +
    geom_jitter() +
    facet_wrap(~ parameter, scales = "free")

## -----------------------------------------------------------------------------
# filter out models which can clearly be rejected
fits <- qpAdm_filter(models)

## ---- fig.width = 6, fig.height = 4-------------------------------------------
select(fits$proportions, model, pvalue, prop1, prop2) %>%
    gather(parameter, value, -model) %>%
    ggplot(aes(parameter, value)) +
    geom_jitter() +
    facet_wrap(~ parameter, scales = "free") +
    coord_cartesian(y = c(0, 1))

## -----------------------------------------------------------------------------
props <- fits$proportions %>%
    arrange(noutgroups) %>%
    select(-c(target, noutgroups, stderr1, stderr2, nsnps_used, nsnps_target))

print(props, n = Inf)

## -----------------------------------------------------------------------------
filter(props, source1 == "Chimp" | source2 == "Chimp")

## -----------------------------------------------------------------------------
filter(props, prop1 < 0.9, prop2 < 0.9)

## -----------------------------------------------------------------------------
loginfo(fits, "m40")

