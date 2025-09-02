# Plot confidence region


ammiBayes.conf.plot <-function(model, conf=0.95, pars.gen=NULL, pars.env=NULL, gen.labels=NULL, env.labels=NULL, 
														col.env="red", col.gen="green", alpha.env=80, alpha.gen=80, col.text.env="black",
														col.text.gen="black",
														border.gen="transparent",
														border.env="transparent", cex.env=1, cex.gen=1,
														lty.gen=1, lty.env=1, lwd.gen=1, lwd.env=1, xlab,
														ylab, col.grid="grey", lty.grid=2, lwd.grid=1,
														change.signal=FALSE,
														plot.gen=TRUE, plot.env=TRUE)

{
	if(!inherits(model, "ammiBayes")) stop("model must be an object ammiBayes")

	l <- model$output[[1]]$L

	# Environment
	v1 <- model$output[[1]]$atv1*sqrt(l[,1])
	v2 <- model$output[[1]]$atv2*sqrt(l[,2])

	# Genotype
	u1 <- model$output[[1]]$atu1*sqrt(l[,1])
	u2 <- model$output[[1]]$atu2*sqrt(l[,2])

# 	gen.m1 <- u1*sqrt(l[,1])  
# 	gen.m2 <- u2*sqrt(l[,2])  
 
# 	amb.m1 <- v1*sqrt(l[,1])
# 	amb.m2 <- v2*sqrt(l[,2])  
	# 
	if(change.signal==TRUE){
		med.v <- cbind(colMeans(v1),colMeans(v2))
		v1c <- svd(med.v)
		amb.m   <- v1c$u%*%t(v1c$v)

		amb.l2 <- amb.m[,2]*sqrt(mean(l[,2]))
		amb.l2 <- amb.l2[1]

		if(amb.l2 <= 0){
			for(i in 1:nrow(v2)){
				if(v2[i,1] > 0){
					v2[i,] <- -v2[i,]
					u2[i,] <- -u2[i,]
				}
			}
		}
		if(amb.l2 > 0){	
			for(i in 1:nrow(v2)){
				if(v2[i,1] < 0){
					v2[i,] <- -v2[i,]
					u2[i,] <- -u2[i,]
				}
			}
		}
	}


	if(length(pars.env)>=1){
		colnames(v1) <- model$info.ammi$envLevels
		colnames(v2) <- model$info.ammi$envLevels

		v1 <- subset(v1, select=pars.env)
		v2 <- subset(v2, select=pars.env)

	} else {
		colnames(v1) <- model$info.ammi$envLevels
		colnames(v2) <- model$info.ammi$envLevels
	}


	if(length(pars.gen)>=1){
		colnames(u1) <- model$info.ammi$lGen
		colnames(u2) <- model$info.ammi$lGen

		u1 <- subset(u1, select=pars.gen)
		u2 <- subset(u2, select=pars.gen)
	} else {
		colnames(u1) <- model$info.ammi$lGen
		colnames(u2) <- model$info.ammi$lGen
	}


	# u = Genotype
	# v = Environment

	# 
# 	gen.m1 <- u1*sqrt(l[,1])  
# 	gen.m2 <- u2*sqrt(l[,2])  
# 
# 	amb.m1 <- v1
# 	amb.m2 <- v2*sqrt(l[,2])  
# 
	amb1.vet <- t(t(as.vector(v1)))
	amb2.vet <- t(t(as.vector(v2)))

	gen1.vet <- t(t(as.vector(u1)))
	gen2.vet <- t(t(as.vector(u2)))


	dat.env <- data.frame(x=amb1.vet, y=amb2.vet, env=rep(colnames(v1), each=nrow(v1)))

	dat.gen <- data.frame(x=gen1.vet, y=gen2.vet, gen=rep(colnames(u1), each=nrow(u1)))


	gen.split <- with(dat.gen, split(dat.gen, gen))

	env.split <- with(dat.env, split(dat.env, env))

	gen.points <- lapply(gen.split, function(x){
												 poi <- distfree.cr(x=x$x, y=x$y, alpha=1-conf, draw=FALSE)
												 data.frame(x=poi$polygon$x, y=poi$polygon$y,
																		gen=x$gen[1])
														})

	gen.graf <- do.call(rbind.data.frame, gen.points)


	env.points <- lapply(env.split, function(x){
												 poi <- distfree.cr(x=x$x, y=x$y, alpha=1-conf, draw=FALSE)
												 data.frame(x=poi$polygon$x, y=poi$polygon$y,
																		env=x$env[1])
														})

	env.graf <- do.call(rbind.data.frame, env.points)

	ylim <- range(c(env.graf$y, gen.graf$y))
	xlim <- range(c(env.graf$x, gen.graf$x))

	px.env <- as.vector(colMeans(v1))
	py.env <- as.vector(colMeans(v2))

	if(is.null(env.labels)){
		env.names <- colnames(v1)
	} else {
		env.names <- env.labels
	}

	px.gen <- as.vector(colMeans(u1))
	py.gen <- as.vector(colMeans(u2))

	if(is.null(gen.labels)){
		gen.names <- colnames(u1)
	} else {
		gen.names <- gen.labels
	}

	color.env <- t_col(col.env, alpha.env)

	color.gen <- t_col(col.gen, alpha.gen)



	if(isTRUE(plot.gen & isTRUE(plot.env))){
			xyplot(y ~ x, data=env.graf, ylim=c(ylim[1], ylim[2]), 
						 xlim=c(xlim[1], xlim[2]), xlab=xlab, ylab=ylab,
						 points.polygon=env.points,
						 px.env=px.env, py.env=py.env, env.names=env.names,
						 color.env=color.env, border.env=border.env, cex.env=cex.env,
						 lty.env=lty.env, lwd.env=lwd.env, col.grid=col.grid, lty.grid=lty.grid, lwd.grid=lwd.grid,
						 col.text.env=col.text.env,
						 panel=function(x, y, points.polygon=points.polygon, px.env=px.env,
														py.env=py.env, env.names=env.names,
														color.env=color.env, border.env=border.env, lty.grid=lty.grid, lwd.grid=lwd.grid,
														cex.env=cex.env, lty.env=lty.env, lwd.env=lwd.env, col.grid=col.grid,
														col.text.env=col.text.env, ...){
							 panel.segments(x0=c(0, -10000), y0=c(-10000,0), x1=c(0,10000),
															y1=c(10000,0), col=col.grid, lty=lty.grid, lwd=lwd.grid, ...)
							 panel.text(x=px.env, y=py.env, labels=env.names, cex=cex.env, col=col.text.env, ...)
							 for(i in 1:length(points.polygon)){
								 panel.polygon(x=points.polygon[[i]]$x, y=points.polygon[[i]]$y,
															 col=color.env, border=border.env, lty=lty.env,
															 lwd=lwd.env, ...)
							 }
						 })+
				as.layer(xyplot(y ~ x, data=gen.graf, 
								points.polygon=gen.points,
								px.gen=px.gen, py.gen=py.gen, gen.names=gen.names,
								color.gen=color.gen, border.gen=border.gen, cex.gen=cex.gen,
								lwd.gen=lwd.gen, lty.gen=lty.gen, col.text.gen=col.text.gen,
								panel=function(x, y, points.polygon=points.polygon,
															 px.gen=px.gen, py.gen=py.gen,
															 gen.names=gen.names, color.gen=color.gen,
															 border.gen=border.gen, cex.gen=cex.gen,
															 lwd.gen=lwd.gen, lty.gen=lty.gen, col.text.gen=col.text.gen,  ...){
									panel.text(x=px.gen, y=py.gen, labels=gen.names, cex=cex.gen, col=col.text.gen, ...)
									for(i in 1:length(points.polygon)){
										panel.polygon(x=points.polygon[[i]]$x, y=points.polygon[[i]]$y,
																	col=color.gen, border=border.gen, lty=lty.gen,
																	lwd=lwd.gen,...)
									}
								}))
	} else if(!isTRUE(plot.env)){
			xyplot(y ~ x, data=gen.graf, ylim=c(ylim[1], ylim[2]), 
						 xlim=c(xlim[1], xlim[2]), xlab=xlab, ylab=ylab,
						 points.polygon=gen.points,
						 px.gen=px.gen, py.gen=py.gen, gen.names=gen.names,
						 color.gen=color.gen, border.gen=border.gen, cex.gen=cex.gen,
						 lwd.gen=lwd.gen, lty.gen=lty.gen, col.text.gen=col.text.gen,
						 panel=function(x, y, points.polygon=points.polygon,
														px.gen=px.gen, py.gen=py.gen,
														gen.names=gen.names, color.gen=color.gen,
														border.gen=border.gen, cex.gen=cex.gen,
														lwd.gen=lwd.gen, lty.gen=lty.gen, col.text.gen=col.text.gen,  ...){
							 panel.text(x=px.gen, y=py.gen, labels=gen.names, cex=cex.gen, col=col.text.gen, ...)
							 for(i in 1:length(points.polygon)){
								 panel.polygon(x=points.polygon[[i]]$x, y=points.polygon[[i]]$y,
															 col=color.gen, border=border.gen, lty=lty.gen,
															 lwd=lwd.gen,...)
							 }
						 })
	} else if(!isTRUE(plot.gen)){
			xyplot(y ~ x, data=env.graf, ylim=c(ylim[1], ylim[2]), 
						 xlim=c(xlim[1], xlim[2]), xlab=xlab, ylab=ylab,
						 points.polygon=env.points,
						 px.env=px.env, py.env=py.env, env.names=env.names,
						 color.env=color.env, border.env=border.env, cex.env=cex.env,
						 lty.env=lty.env, lwd.env=lwd.env, col.grid=col.grid, lty.grid=lty.grid, lwd.grid=lwd.grid,
						 col.text.env=col.text.env,
						 panel=function(x, y, points.polygon=points.polygon, px.env=px.env,
														py.env=py.env, env.names=env.names,
														color.env=color.env, border.env=border.env, lty.grid=lty.grid, lwd.grid=lwd.grid,
														cex.env=cex.env, lty.env=lty.env, lwd.env=lwd.env, col.grid=col.grid,
														col.text.env=col.text.env, ...){
							 panel.segments(x0=c(0, -10000), y0=c(-10000,0), x1=c(0,10000),
															y1=c(10000,0), col=col.grid, lty=lty.grid, lwd=lwd.grid, ...)
							 panel.text(x=px.env, y=py.env, labels=env.names, cex=cex.env, col=col.text.env, ...)
							 for(i in 1:length(points.polygon)){
								 panel.polygon(x=points.polygon[[i]]$x, y=points.polygon[[i]]$y,
															 col=color.env, border=border.env, lty=lty.env,
															 lwd=lwd.env, ...)
							 }
						 })
	}

}
		

# Plot means

ammiBayes.mean.plot <-function(model, pars.gen=NULL, pars.env=NULL, gen.labels=NULL, env.labels=NULL, 
										 col.text.gen="darkgreen", col.text.env="red", ylim=NULL,
										 xlim=NULL, cex.env=1, cex.gen=1,
										 xlab,	ylab, col.grid="grey", lty.grid=2, lwd.grid=1)

{
	if(!inherits(model, "ammiBayes")) stop("model must be an object ammiBayes")

	l <- model$output[[1]]$L

	# Environment
	v1 <- model$output[[1]]$atv1
	v2 <- model$output[[1]]$atv2


	# Genotype
	u1 <- model$output[[1]]$atu1
	u2 <- model$output[[1]]$atu2

	if(length(pars.env)>=1){
		colnames(v1) <- model$info.ammi$envLevels
		colnames(v2) <- model$info.ammi$envLevels

		v1 <- subset(v1, select=pars.env)
		v2 <- subset(v2, select=pars.env)

	} else {
		colnames(v1) <- model$info.ammi$envLevels
		colnames(v2) <- model$info.ammi$envLevels
	}


	if(length(pars.gen)>=1){
		colnames(u1) <- model$info.ammi$lGen
		colnames(u2) <- model$info.ammi$lGen

		u1 <- subset(u1, select=pars.gen)
		u2 <- subset(u2, select=pars.gen)
	} else {
		colnames(u1) <- model$info.ammi$lGen
		colnames(u2) <- model$info.ammi$lGen
	}

	med.v <- cbind(colMeans(v1),colMeans(v2))
	v1c <- svd(med.v)
	amb.m   <- v1c$u%*%t(v1c$v)

	amb.l1 <- amb.m[,1]*sqrt(mean(l[,1]))
	amb.l2 <- amb.m[,2]*sqrt(mean(l[,2]))

	if(is.null(env.labels)){
		env.names <-  colnames(v1)
	} else {
		env.names <- env.labels
	}

	med.u <- cbind(colMeans(u1),colMeans(u2))
	u1c <- svd(med.u)
	gen.m   <- u1c$u%*%t(u1c$v)

	gen.l1 <- gen.m[,1]*sqrt(mean(l[,1]))
	gen.l2 <- gen.m[,2]*sqrt(mean(l[,2]))

	if(is.null(gen.labels)){
		gen.names <-  colnames(u1)
	} else {
		gen.names <- gen.labels
	}

	if(is.null(xlim)){
		xlim <- range(c(amb.l1, gen.l1))
	} else {
		xlim <- xlim
	}

	if(is.null(ylim)){
		ylim <- range(c(amb.l2, gen.l2))
	} else {
		ylim <- xlim
	}

	xyplot(amb.l2 ~ amb.l1, ylim=c(ylim[1]+ylim[1]/10, ylim[2]+ylim[2]/10), 
				 xlim=c(xlim[1]+xlim[1]/10, xlim[2]+xlim[2]/10), xlab=xlab, ylab=ylab,
				 env.names=env.names, cex.env=cex.env,
				 col.grid=col.grid, lty.grid=lty.grid, lwd.grid=lwd.grid,
				 col.text.env=col.text.env, 
				 panel=function(x, y, 
												env.names=env.names,
												lty.grid=lty.grid, lwd.grid=lwd.grid,
												cex.env=cex.env, col.grid=col.grid,
												col.text.env=col.text.env, pch.env=pch.env, ...){
					 panel.segments(x0=c(0, -10000), y0=c(-10000,0), x1=c(0,10000),
													y1=c(10000,0), col=col.grid, lty=lty.grid, lwd=lwd.grid, ...)
					 panel.text(x=x, y=y, labels=env.names, cex=cex.env, col=col.text.env, ...)
				 })+
	as.layer(xyplot(gen.l2 ~ gen.l1,  
									gen.names=gen.names,
									cex.gen=cex.gen,
									col.text.gen=col.text.gen,
									panel=function(x, y,
																 gen.names=gen.names, cex.gen=cex.gen, col.text.gen=col.text.gen,  ...){
										panel.text(x=x, y=y, labels=gen.names, cex=cex.gen, col=col.text.gen, ...)
									}))

}



ammiBayes.gen.plot <- function(x, lwd=1, lty=1, pch=18, method="bars",
															 col=NULL, ylim=NULL, draw.mean=TRUE, col.mean="red",
															 lty.mean=2, xlab="Genotype", ylab=NULL, gen.names=NULL)
{
	if(!inherits(x, "ammiBayes")) stop("model must be an object ammiBayes")

	xdat <- x[,c(1,3,4)]
	colnames(xdat) <- c("Mean","lower","upper")
	
	tam.names <- nrow(xdat)
	
	if(is.null(gen.names)){
		g.names <- rownames(x)
	}
	else {
		g.names <- gen.names
	}
	
	xdat[["vetx"]] <- 1:nrow(xdat)

	if(draw.mean!=TRUE){ col.mean="transparent"}

	med.pop <- mean(xdat$Mean)
	
		
	xYplot(Cbind(Mean, lower, upper) ~ vetx, data=xdat, lwd=lwd, lty=lty,
				 pch=pch, method=method, col=col, ylim=ylim, xlab=xlab, ylab=ylab,
				 scales=list(x=list(at=1:tam.names, labels=g.names)),
				 panel=function(...){
					 panel.xYplot(...)
					 panel.segments(x0=-1, y0=med.pop, x1=nrow(xdat)+1, y1=med.pop, col=col.mean, lty=lty.mean)
				 }
				 )

}


## Transparent colors
## Mark Gardener 2015
## www.dataanalytics.org.uk

t_col <- function(color, percent) {

	rgb.val <- col2rgb(color)

	t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
							 maxColorValue = 255,
							 alpha = (100 - percent) * 255 / 100)

	invisible(t.col)
}

