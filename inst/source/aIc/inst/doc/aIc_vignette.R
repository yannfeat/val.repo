## ---- echo=F, message=F, warning=F--------------------------------------------

library(aIc)
library(matrixcalc)
library(edgeR)
data(selex)
group <- c(rep('n',7), rep('s',7))
oldrow <- par(mfrow=c(1,1))

## ---- echo=T------------------------------------------------------------------
test.cor.clr <- aIc.coherent(selex, group=group, zero.method='prior', 
  norm.method='clr', log=F, cor.test='spearman')
test.cor.TMM <- aIc.coherent(selex, group=group, zero.method='prior', 
  norm.method='TMM', log=F, cor.test='spearman')
test.cor.clr$is.coherent
test.cor.TMM$is.coherent

# plot the results
par(mfrow=c(1,2))
aIc.plot(test.cor.clr)
aIc.plot(test.cor.TMM)
par(oldrow)

## ---- echo=T------------------------------------------------------------------

test.dom.clr <- aIc.dominant(selex, group=group, zero.method='prior', 
  norm.method='clr', log=F)
test.dom.TMM <- aIc.dominant(selex, group=group, zero.method='prior', 
  norm.method='TMM', log=T)
test.dom.clr$is.dominant
test.dom.TMM$is.dominant

# plot the results
par(mfrow=c(1,2))
aIc.plot(test.dom.clr)
aIc.plot(test.dom.TMM)
par(oldrow)

## ---- echo=T------------------------------------------------------------------
test.scale.clr <- aIc.scale(selex, group=group, zero.method='GBM', 
  norm.method='clr', log=F)
test.scale.TMM <- aIc.scale(selex, group=group, zero.method='GBM', 
  norm.method='TMM', log=T)
test.scale.clr$is.scale
test.scale.TMM$is.scale
par(mfrow=c(1,2))
aIc.plot(test.scale.clr)
aIc.plot(test.scale.TMM)
par(oldrow)

## ---- echo=T------------------------------------------------------------------

test.perturb.clr <- aIc.perturb(selex, group=group, zero.method='prior', 
  norm.method='clr', log=F)
test.perturb.TMM <- aIc.perturb(selex, group=group, zero.method='prior', 
  norm.method='RLE', log=F)
test.perturb.clr$is.perturb
test.perturb.TMM$is.perturb
par(mfrow=c(1,2))
aIc.plot(test.perturb.clr)
aIc.plot(test.perturb.TMM)
par(oldrow)

## -----------------------------------------------------------------------------
sessionInfo()

