## ----setup, include = FALSE---------------------------------------------------
evaluate <- .Platform$OS.type == "unix" && system("which qpDstat", ignore.stdout = TRUE) == 0

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "",
  eval = evaluate
)

## ---- eval = FALSE------------------------------------------------------------
#  install.packages("devtools")
#  devtools::install_github("bodkan/admixr")

## ---- eval = FALSE------------------------------------------------------------
#  install.packages("tidyverse")

## ----libraries, message = FALSE, warning = FALSE------------------------------
library(admixr)
library(tidyverse)

## ----eigenstrat_path----------------------------------------------------------
(prefix <- download_data(dirname = tempdir()))

## ----eigenstrat_trio----------------------------------------------------------
list.files(path = dirname(prefix), pattern = basename(prefix), full.names = TRUE)

## ----ind_file, echo = FALSE---------------------------------------------------
cat(system(paste0("column -t ", prefix, ".ind"), intern = TRUE), sep = "\n")

## ----snp_file, echo = FALSE---------------------------------------------------
cat(system(paste0("head -n 3 ", prefix, ".snp"), intern = TRUE), sep = "\n")

## ----geno_file, echo = FALSE--------------------------------------------------
cat(system(paste0("head -n 3 ", prefix, ".geno"), intern = TRUE), sep = "\n")

## -----------------------------------------------------------------------------
snps <- eigenstrat(prefix)

## ---- comment = "#>"----------------------------------------------------------
snps

## ----pop_def1-----------------------------------------------------------------
pops <- c("French", "Sardinian", "Han", "Papuan", "Khomani_San", "Mbuti", "Dinka")

## ----d------------------------------------------------------------------------
result <- d(W = pops, X = "Yoruba", Y = "Vindija", Z = "Chimp", data = snps)

## ----eval = FALSE-------------------------------------------------------------
#  head(result)

## ----d_kable, echo = FALSE----------------------------------------------------
knitr::kable(head(result))

## ----d_plot, fig.width = 7, fig.height = 4------------------------------------
ggplot(result, aes(fct_reorder(W, D), D, color = abs(Zscore) > 2)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_errorbar(aes(ymin = D - 2 * stderr, ymax = D + 2 * stderr))

## ----f4-----------------------------------------------------------------------
result <- f4(W = pops, X = "Yoruba", Y = "Vindija", Z = "Chimp", data = snps)

## ----eval = FALSE-------------------------------------------------------------
#  head(result)

## ----f4_kable, echo = FALSE---------------------------------------------------
knitr::kable(head(result))

## ----f4ratio------------------------------------------------------------------
result <- f4ratio(X = pops, A = "Altai", B = "Vindija", C = "Yoruba", O = "Chimp", data = snps)

## ---- eval=FALSE--------------------------------------------------------------
#  head(result)

## ----f4ratio_kable, echo=FALSE------------------------------------------------
knitr::kable(head(result))

## ----f4ratio_plot, fig.width = 7, fig.height = 4------------------------------
ggplot(result, aes(fct_reorder(X, alpha), alpha, color = abs(Zscore) > 2)) +
  geom_point() +
  geom_errorbar(aes(ymin = alpha - 2 * stderr, ymax = alpha + 2 * stderr)) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(y = "Neandertal ancestry proportion", x = "present-day individual")

## ----pops2--------------------------------------------------------------------
pops <- c("French", "Sardinian", "Han", "Papuan", "Mbuti", "Dinka", "Yoruba")

result <- f3(A = pops, B = pops, C = "Khomani_San", data = snps)

## ---- eval=FALSE--------------------------------------------------------------
#  head(result)

## ----f3_kable, echo=FALSE-----------------------------------------------------
knitr::kable(head(result))

## ----f3_plot, fig.width = 8, fig.height = 6-----------------------------------
# sort the population labels according to an increasing f3 value relative to French
ordered <- filter(result, A == "Mbuti", B != "Mbuti") %>% arrange(f3) %>% .[["B"]] %>% c("Mbuti")

# plot heatmap of pairwise f3 values
result %>%
  filter(A != B) %>%
  mutate(A = factor(A, levels = ordered),
         B = factor(B, levels = ordered)) %>%
  ggplot(aes(A, B)) + geom_tile(aes(fill = f3))

## -----------------------------------------------------------------------------
result <- qpWave(
 left = c("French", "Sardinian", "Han"),
 right = c("Altai", "Yoruba", "Mbuti"),
 data = snps
)

## ---- eval = FALSE------------------------------------------------------------
#  result

## ---- echo = FALSE------------------------------------------------------------
knitr::kable(result)

## -----------------------------------------------------------------------------
result <- qpWave(
 left = c("Papuan", "French", "Sardinian", "Han"),
 right = c("Altai", "Yoruba", "Mbuti"),
 data = snps
)

## ---- eval = FALSE------------------------------------------------------------
#  result

## ---- echo = FALSE------------------------------------------------------------
knitr::kable(result)

## ----qpAdm--------------------------------------------------------------------
result <- qpAdm(
  target = c("Sardinian", "Han", "French"),
  sources = c("Vindija", "Yoruba"),
  outgroups = c("Chimp", "Denisova", "Altai"),
  data = snps
)

## ---- eval = FALSE------------------------------------------------------------
#  result$ranks

## ---- echo = FALSE------------------------------------------------------------
knitr::kable(result$ranks)

## ---- eval = FALSE------------------------------------------------------------
#  result$proportions

## ---- echo=FALSE--------------------------------------------------------------
knitr::kable(result$proportions)

## ---- eval = FALSE------------------------------------------------------------
#  result$subsets

## ---- echo=FALSE--------------------------------------------------------------
knitr::kable(result$subsets)

## ----orig_ind, echo = FALSE, comment = ""-------------------------------------
cat(system(paste0("column -t ", snps$ind), intern = TRUE), sep = "\n")

## ----relabel------------------------------------------------------------------
# paths to the original ind file and a new modified ind file, which will
# contain merged population labels
modif_snps <- relabel(
  snps,
  European = c("French", "Sardinian"),
  African = c("Dinka", "Yoruba", "Mbuti", "Khomani_San"),
  Archaic = c("Vindija", "Altai", "Denisova")
)

## ---- comment = "#>"----------------------------------------------------------
modif_snps

## ----modif_ind, echo = FALSE, comment = ""------------------------------------
cat(system(paste0("column -t ", modif_snps$group), intern = TRUE), sep = "\n")

## ----modif_d------------------------------------------------------------------
result <- d(W = "European", X = "African", Y = "Archaic", Z = "Chimp", data = modif_snps)

## ----eval = FALSE-------------------------------------------------------------
#  head(result)

## ----modif_d_kable, echo = FALSE----------------------------------------------
knitr::kable(head(result))

## ----present_snps, results = "hide"-------------------------------------------
count_snps(snps)

## ----present_snps_kable, echo = FALSE-----------------------------------------
knitr::kable(count_snps(snps))

## -----------------------------------------------------------------------------
bed <- file.path(dirname(prefix), "regions.bed")

## -----------------------------------------------------------------------------
# BED file contains regions to keep in an analysis
new_snps <- filter_bed(snps, bed)

# BED file contains regions to remove from an analysis
new_snps <- filter_bed(snps, bed, remove = TRUE)

## ---- comment = "#>"----------------------------------------------------------
new_snps

## ---- eval = FALSE------------------------------------------------------------
#  snps %>%
#    filter_bed("regions.bed") %>%
#    d(W = "French", X = "Mbuti", Y = "Vindija", Z = "Chimp")

## ---- eval = FALSE------------------------------------------------------------
#  new_snps <- transversions_only(snps)
#  
#  # perform the calculation only on transversions
#  d(W = "French", X = "Dinka", Y = "Altai", Z = "Chimp", data = new_snps)

## ---- eval = FALSE------------------------------------------------------------
#  snps %>%                                    # take the original data
#    filter_bed("regions.bed", remove = TRUE) %>%  # remove sites not in specified regions
#    transversions_only() %>%                      # remove potential false SNPs due to aDNA damage
#    d(W = "French", X = "Dinka", Y = "Altai", Z = "Chimp") # calculate D on the filtered dataset

## ----merge_eigenstrat, eval = FALSE-------------------------------------------
#  # this is just an example code - it will not run unless you specify the paths
#  merged <- merge_eigenstrat(
#      merged = <"prefix of the merged dataset">
#      a = first_EIGENSTRAT_object,
#      b = second_EIGENSTRAT_object
#  )

## ----d_log, comment = "#>"----------------------------------------------------
dres <- d(W = c("French", "Han", "Dinka"), X = "Yoruba", Y = "Vindija", Z = "Chimp", data = snps)

dres

## ----d_loginfo, comment = "#>"------------------------------------------------
loginfo(dres)

## ----qpAdm_log, comment = "#>"------------------------------------------------
qpadm_res <- qpAdm(
  target = c("Sardinian", "Han"),
  sources = c("Vindija", "Yoruba"),
  outgroups = c("Chimp", "Denisova", "Altai"),
  data = snps
)

qpadm_res

## ----qpadm_log_target, comment = "#>"-----------------------------------------
loginfo(qpadm_res, target = "Han")

## ----qpadm_log_save-----------------------------------------------------------
loginfo(qpadm_res, target = "Sardinian", save = TRUE, prefix = "qpAdm_Neandertal_ancestry")

