# Fitness statistics graph by GA generations
plotfitness = function(genfits, options){
  if(missing(options)) options=c(3,2)
  if(is.vector(genfits))
    genfits = matrix(genfits, nrow=1, ncol=8)
  x1 = x2 = x3 = x4 = NULL
  lbls = c("min","max","avg","Q1","med","Q3")
  clrs = c("gray","blue","red", "pink","cyan","green")
  ltys = c(3,2,1,4,5,6)
  x1 = genfits[,options[1]]
  if(length(options)==1)
    ltys[options] = 1
  if(length(options)==2)
    x2=genfits[,options[2]]
  else if(length(options)==3){
    x2=genfits[,options[2]]
    x3=genfits[,options[3]]
  }else if(length(options)==4){
    x2=genfits[,options[2]]
    x3=genfits[,options[3]]
    x4=genfits[,options[4]]
  }
  plot(x1, col=clrs[options[1]], type="l", 
       lwd=5, lty=ltys[options[1]], 
       ylim=c(min(genfits[,1]), max(genfits[,2])),
       xlab="Iterations", ylab="Fitness value",
       main="Fitness value by iterations")
  lines(x1, col=7, lwd=1, lty=1)
  if(!is.null(x2))
    lines(x2, col=clrs[options[2]], lwd=2, lty=ltys[options[2]])
  if(!is.null(x3))
    lines(x3, col=clrs[options[3]], lwd=2, lty=ltys[options[3]])
  if(!is.null(x4))
    lines(x4, col=clrs[options[4]], lwd=2, lty=ltys[options[4]])
  legend("bottom", inset=.02, 
         lbls[options], col=clrs[options], 
         lty=ltys[options], horiz=TRUE, cex=0.8)
}