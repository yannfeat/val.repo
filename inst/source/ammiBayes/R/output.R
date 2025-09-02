summary.ammiBayes <- function(object, ...){
	if(!inherits(object, "ammiBayes")) stop("'object' must be an object ammiBayes")


	medias <- colMeans(object$output[[1]]$cpvar)
	sdc <- apply(object$output[[1]]$cpvar,2,sd)
	limites <- HPDinterval(mcmc(object$output[[1]]$cpvar))


	compvar <- data.frame(Mean=round(medias,4), St.dev=round(sdc,4), 
												LI=round(limites[,1],4), LS=round(limites[,2],4))
	rownames(compvar) <- c("Var.gen", "Var.error")
	colnames(compvar) <- c("Mean", "St.dev", "LI(2.5%)", "LS(97.5%)")

	medias.lamb <- colMeans(object$output[[1]]$L)
	sdc.lamb <- apply(object$output[[1]]$L,2,sd)
	limites.lamb <- HPDinterval(mcmc(object$output[[1]]$L))


	lambda <- data.frame(Mean=round(medias.lamb,4), St.dev=round(sdc.lamb,4), 
												LI=round(limites.lamb[,1],4), LS=round(limites.lamb[,2],4))
	colnames(lambda) <- c("Mean", "St.dev", "LI(2.5%)", "LS(97.5%)")


	cat("Additive Main Effects and Multiplicative Interaction Model", "\n")

	cat("\n")

	cat("Total of genotypes:", object$info.ammi$nGen, "\n")

	cat("\n")


	cat("Random effects", "\n")
	print(compvar)

	cat("\n")

	cat("Lambda", "\n")
	print(lambda)

	cat("\n")

	print(t(data.frame("Acumulated lambda" = cumsum(medias.lamb),
										 row.names=paste("L", 1:length(medias.lamb), sep="."))))

	cat("\n")
	cat("Explained","\n")
	per    <- t(data.frame(Percentage=medias.lamb^2/sum(medias.lamb^2),
												 Perc.acumulated=per.ac <- (cumsum(medias.lamb^2)/sum(medias.lamb^2)),
												 row.names=paste("D", 1:length(medias.lamb), sep=".")))
	print(per)

	cat("\n")

	cat("Elapsed time:", paste(round(object$output[[1]]$time.el[3]/60,3), "minutes", sep=" "), "\n")

	cat("\n")

	cat("Iterations:", object$info.iter$iterations, "Jump:", object$info.iter$jump, "Burn:", object$info.iter$burn, "\n")

}

predict.ammiBayes <- function(object, prob=0.95, ...){
	if(!inherits(object, "ammiBayes")) stop("'object' must be an object ammiBayes")

  result  <- mcmc(object$output[[1]]$pred.chain)

	means <- rowMeans(result)

	mediana <- apply(result,1,median)

	intervals <- apply(result, 1, function(x) HPDinterval(mcmc(x), prob=prob))

	intervals <- t(intervals)

	output <- data.frame(means, mediana, intervals)

	colnames(output) <- c("Mean", "Median", paste((1-prob)/2*100,"%",sep=""),
												paste((1-((1-prob)/2))*100, "%",sep=""))

# 	class(output) <- "ammiBayes.predict"

	return(output)
}



gen.effects <- function(x, prob=0.95){
	if(!inherits(x, "ammiBayes")) stop("x must be an object ammiBayes")

  result  <- mcmc(x$output[[1]]$gen.chain)

	means <- colMeans(result)

	mediana <- apply(result,2,median)

	intervals <- HPDinterval(result, prob=prob)

	output <- data.frame(means, mediana, intervals)

	colnames(output) <- c("Mean", "Median", paste((1-prob)/2*100,"%",sep=""),
												paste((1-((1-prob)/2))*100, "%",sep=""))

	output <- output[do.call(order, output),]

	class(output) <- c("ammiBayes", "data.frame")

	return(output)

}




diagnosis.ammiBayes <- function(x, pars=NULL){

	if(!inherits(x, "ammiBayes")) stop("x must be an object ammiBayes")
	if(is.null(pars)) stop("Argument 'pars' must be defined")
	if(length(pars) != 1) stop("Argument 'pars' should be length = 1")

	if(pars=="Genotype"){
		result <- x$output
		out <- lapply(result, function(x) mcmc(x$gen.chain))
		return(as.mcmc.list(out))
	}
	if(pars=="Rep"){
		result <- x$output
		out <- lapply(result, function(x) mcmc(x$rep.chain))
		return(as.mcmc.list(out))
	}
	if(pars=="L"){
		result <- x$output
		out <- lapply(result, function(x) mcmc(x$L))
		return(as.mcmc.list(out))
	}
	if(pars=="Gen.PC1"){
		result <- x$output
		out <- lapply(result, function(x) mcmc(x$atu1))
		return(as.mcmc.list(out))
	}
	if(pars=="Gen.PC2"){
		result <- x$output
		out <- lapply(result, function(x) mcmc(x$atu2))
		return(as.mcmc.list(out))
	}
	if(pars=="Env.PC1"){
		result <- x$output
		out <- lapply(result, function(x) mcmc(x$atv1))
		return(as.mcmc.list(out))
	}
	if(pars=="Env.PC2"){
		result <- x$output
		out <- lapply(result, function(x) mcmc(x$atv2))
		return(as.mcmc.list(out))
	}
	if(pars=="Comp.var"){
		result <- x$output
		out <- lapply(result, function(x) mcmc(x$cpvar))
		return(as.mcmc.list(out))
	}
}


