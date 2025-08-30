## ---- echo = FALSE------------------------------------------------------------
library(knitr)
opts_chunk$set(fig.width = 6, fig.height = 4)

## -----------------------------------------------------------------------------
library(adaptiveGPCA)
library(ggplot2)
library(phyloseq)
data(AntibioticPhyloseq)
theme_set(theme_bw())

## -----------------------------------------------------------------------------
pp = processPhyloseq(AntibioticPhyloseq)

## -----------------------------------------------------------------------------
out.agpca = adaptivegpca(pp$X, pp$Q, k = 2)

## -----------------------------------------------------------------------------
out.agpca

## -----------------------------------------------------------------------------
plot(out.agpca)

## -----------------------------------------------------------------------------
plot(out.agpca, type = "samples", axes = c(1,2))
plot(out.agpca, type = "variables", axes = c(1,2))

## ----eval = FALSE-------------------------------------------------------------
#  inspectTaxonomy(out.agpca, AntibioticPhyloseq)

## ---- eval = FALSE------------------------------------------------------------
#  out.ff = gpcaFullFamily(pp$X, pp$Q, k = 2)
#  out.agpca = visualizeFullFamily(out.ff,
#                      sample_data = sample_data(AntibioticPhyloseq),
#                      sample_mapping = aes(x = Axis1, y = Axis2, color = type),
#                      var_data = tax_table(AntibioticPhyloseq),
#                      var_mapping = aes(x = Axis1, y = Axis2, color = Phylum))

## ----fig.width=7--------------------------------------------------------------
ggplot(data.frame(out.agpca$U, sample_data(AntibioticPhyloseq))) +
    geom_point(aes(x = Axis1, y = Axis2, color = type, shape = ind)) +
    xlab("Axis 1") + ylab("Axis 2")

## ----fig.width=9--------------------------------------------------------------
ggplot(data.frame(out.agpca$QV, tax_table(AntibioticPhyloseq))) +
    geom_point(aes(x = Axis1, y = Axis2, color = Phylum)) +
    xlab("Axis 1") + ylab("Axis 2")

