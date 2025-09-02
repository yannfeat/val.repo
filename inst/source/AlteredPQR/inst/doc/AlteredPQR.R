### R code from vignette source 'AlteredPQR.Rnw'

###################################################
### code chunk number 1: AlteredPQR.Rnw:16-17
###################################################
library(AlteredPQR)


###################################################
### code chunk number 2: AlteredPQR.Rnw:22-24
###################################################
data("int_pairs", package = "AlteredPQR")
data("quant_data_all", package = "AlteredPQR")


###################################################
### code chunk number 3: AlteredPQR.Rnw:34-35
###################################################
cols_with_reference_data = 1:23


###################################################
### code chunk number 4: AlteredPQR.Rnw:40-41
###################################################
RepresentativePairs = AlteredPQR_RB()


###################################################
### code chunk number 5: AlteredPQR.Rnw:46-47
###################################################
head (RepresentativePairs)


###################################################
### code chunk number 6: AlteredPQR.Rnw:65-67
###################################################
samplesGroupA = 1:23
samplesGroupB = (1+23):(23+18)


###################################################
### code chunk number 7: AlteredPQR.Rnw:72-73
###################################################
cor_results = CorShift()


###################################################
### code chunk number 8: AlteredPQR.Rnw:78-79
###################################################
head (cor_results)


