## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE
)

## ----setup--------------------------------------------------------------------
library(admix)

## ----sym----------------------------------------------------------------------
## Simulate mixture data:
mixt1 <- twoComp_mixt(n = 400, weight = 0.7,
                      comp.dist = list("norm", "norm"),
                      comp.param = list(c("mean" = 3, "sd" = 0.5),
                                        c("mean" = 0, "sd" = 1)))
data1 <- get_mixture_data(mixt1)
## Define the admixture model:
admixMod <- admix_model(knownComp_dist = mixt1$comp.dist[[2]],
                        knownComp_param = mixt1$comp.param[[2]])
admix_estim(samples = list(data1), admixMod = list(admixMod), est_method = 'BVdk')

## ----generalCase--------------------------------------------------------------
admix_estim(samples = list(data1), admixMod = list(admixMod), est_method = 'PS')

## ----twoSampleNullHyp---------------------------------------------------------
## Simulate mixture data:
mixt1 <- twoComp_mixt(n = 450, weight = 0.4,
                      comp.dist = list("norm", "norm"),
                      comp.param = list(list("mean" = -2, "sd" = 0.5),
                                        list("mean" = 0, "sd" = 1)))
mixt2 <- twoComp_mixt(n = 380, weight = 0.7,
                      comp.dist = list("norm", "norm"),
                      comp.param = list(list("mean" = -2, "sd" = 0.5),
                                        list("mean" = 1, "sd" = 1)))
data1 <- get_mixture_data(mixt1)
data2 <- get_mixture_data(mixt2)
## Define the admixture models:
admixMod1 <- admix_model(knownComp_dist = mixt1$comp.dist[[2]],
                         knownComp_param = mixt1$comp.param[[2]])
admixMod2 <- admix_model(knownComp_dist = mixt2$comp.dist[[2]],
                         knownComp_param = mixt2$comp.param[[2]])
admix_estim(samples = list(data1, data2), admixMod = list(admixMod1, admixMod2),
            est_method = 'IBM')

## ----twoSampleAltHyp----------------------------------------------------------
## Simulate mixture data:
mixt1 <- twoComp_mixt(n = 800, weight = 0.5,
                      comp.dist = list("norm", "norm"),
                      comp.param = list(list("mean" = 1, "sd" = 0.5),
                                        list("mean" = 0, "sd" = 1)))
mixt2 <- twoComp_mixt(n = 600, weight = 0.7,
                      comp.dist = list("norm", "norm"),
                      comp.param = list(list("mean" = 3, "sd" = 0.5),
                                        list("mean" = 5, "sd" = 2)))
data1 <- get_mixture_data(mixt1)
data2 <- get_mixture_data(mixt2)
## Define the admixture models:
admixMod1 <- admix_model(knownComp_dist = mixt1$comp.dist[[2]],
                         knownComp_param = mixt1$comp.param[[2]])
admixMod2 <- admix_model(knownComp_dist = mixt2$comp.dist[[2]],
                         knownComp_param = mixt2$comp.param[[2]])
## Estimate the mixture weights of the two admixture models (provide only hat(theta)_n):
admix_estim(samples = list(data1, data2), admixMod = list(admixMod1, admixMod2),
            est_method = 'IBM')

## -----------------------------------------------------------------------------
admix_estim(samples = list(data1, data2), admixMod = list(admixMod1, admixMod2),
            est_method = 'PS')

## ----decontDens---------------------------------------------------------------
## Simulate mixture data:
mixt1 <- twoComp_mixt(n = 800, weight = 0.4,
                      comp.dist = list("norm", "norm"),
                      comp.param = list(list("mean" = 3, "sd" = 0.5),
                                        list("mean" = 0, "sd" = 1)))
mixt2 <- twoComp_mixt(n = 700, weight = 0.6,
                      comp.dist = list("norm", "norm"),
                      comp.param = list(list("mean" = 3, "sd" = 0.5),
                                        list("mean" = 5, "sd" = 2)))
data1 <- get_mixture_data(mixt1)
data2 <- get_mixture_data(mixt2)
## Define the admixture models:
admixMod1 <- admix_model(knownComp_dist = mixt1$comp.dist[[2]],
                         knownComp_param = mixt1$comp.param[[2]])
admixMod2 <- admix_model(knownComp_dist = mixt2$comp.dist[[2]],
                         knownComp_param = mixt2$comp.param[[2]])
## Estimation:
est <- admix_estim(samples = list(data1,data2), admixMod = list(admixMod1,admixMod2),
                   est_method = 'PS')
prop <- get_mixing_weights(est)
## Determine the decontaminated version of the unknown density by inversion:
res1 <- decontaminated_density(sample1 = data1, estim.p = prop[1], admixMod = admixMod1)
res2 <- decontaminated_density(sample1 = data2, estim.p = prop[2], admixMod = admixMod2)
## Use appropriate sequence of x values:
plot(x = res1, x_val = seq(from = 0, to = 6, length.out = 100), add_plot = FALSE)
plot(x = res2, x_val = seq(from = 0, to = 6, length.out = 100), add_plot = TRUE, col = "red")

