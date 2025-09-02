ammiBayes <- function(Y=Y, Gen=Gen, Env=Env, Rep=Rep, iterations=3000, jump=2, burn=500,
											Var.error=0.5, Var.env=0.5, Var.gen=0.5,
											chains=2){

	if(!is.factor(Gen)) stop("Gen is not a factor")
	if(!is.factor(Rep)) stop("Rep is not a factor")
	if(!is.factor(Env)) stop("Env is not a factor")
	
	if(nlevels(Env)<4) stop("Env must have more than 3 environments")

	niter <- iterations*jump + burn

	osp <- Sys.info()['sysname']

	if(osp!="Linux"){

	cat("Operational system detected:", osp, "\n")

	cat("The code will be run in serial mode. Only 1 core will be used.", "\n")

	output <- lapply(1:chains, function(cadeia){
											 outp <- main.sample(Y=Y, Gen=Gen, Env=Env, Rep=Rep,
																					 iter=iterations, jump=jump,
																					 burn=burn, Var.error=Var.error,
																					 Var.env=Var.env, Var.gen=Var.gen,
																					 cadeia=cadeia)
											 return(outp)
											})
	cat("\n")
	}
	else {

	output <- mclapply(1:chains, mc.cores=chains, function(cadeia){
											 outp <- main.sample(Y=Y, Gen=Gen, Env=Env, Rep=Rep,
																					 iter=iterations, jump=jump,
																					 burn=burn, Var.error=Var.error,
																					 Var.env=Var.env, Var.gen=Var.gen,
																					 cadeia=cadeia)
											 return(outp)
											})
	cat("\n")
	}

	info.iter <- list(iterations=iterations, jump=jump, burn=burn)
	rGen <- tapply(Gen,Gen,length)
	info.ammi <- list(Y=Y, nRep=nlevels(Rep), nGen=nlevels(Gen), lRep=levels(Rep),
										lGen=levels(Gen), envLevels=levels(Env))


	res <- list(output=output, info.iter=info.iter, rGen=rGen, info.ammi=info.ammi)

	class(res) <- "ammiBayes"
	return(res)
}

main.sample <- function(Y=NULL, Gen=NULL, Env=NULL, Rep=NULL, iter=iter, jump=jump,
												Var.error=0.5, Var.env=0.5, Var.gen=0.5,
												burn=burn, cadeia=cadeia){

	x  <- Env
	x1 <- Rep
	z  <- Gen

	x  <- model.matrix(~x -1)
	x1 <- model.matrix(~x1 -1)
	z  <- model.matrix(~z -1)
	z1 <- model.matrix(~z*x -1)

	y  <- as.spam(Y)

	namb  <- ncol(x)
	ngen  <- ncol(z)
	nbloc <- ncol(x1)
	n <- nrow(z)
	s <- min(namb,ngen)-1

	ve  <- Var.error
	va  <- Var.env
	vge <- Var.gen

	A1 <- diag.spam(ngen)*(ve/va)
	A2 <- diag.spam(ngen*namb)*(vge/va)


	z1.sp <- as.spam(z1)
	z.sp <- as.spam(z) # Sparse matrix
	y.sp <- as.spam(y)
	x1.sp <- as.spam(x1)
	x.sp <- as.spam(x)

	y <- as.spam(y)

	x1x <- crossprod.spam(x1)
	sx1x <- solve(x1x)
	stx <- tcrossprod.spam(sx1x,x1.sp)
	beta <- stx%*%y


	tzz <- crossprod.spam(z.sp)
	tzzA1 <- tzz+A1
	tzs <- solve(tzzA1)
	tzsz <- tcrossprod.spam(tzs,z.sp)

	x1beta <- x1.sp%*%beta
	yx1beta <- y-x1beta
	g <- tzsz%*%yx1beta

	z1.sp <- z1.sp[,(ngen+namb+1):ncol(z1.sp)]


	pa1 <- crossprod.spam(z1.sp)
	pa1 <- pa1+A2
	part1 <- solve(pa1)
	part1z1 <- tcrossprod.spam(part1,z1.sp)


	zg <- z.sp%*%g

	part2 <- yx1beta-zg
	ge <- part1z1%*%part2

	gem  <- as.spam(matrix(0,ngen,namb))


	for(j in 1:namb)
	{
		if (j==1) gem[,j]=ge[j:ngen] else gem[,j]=ge[((j-1)*ngen+1):(ngen*j)] 
	}


	dec<- svd(gem)
	U  <- dec$u
	L  <- dec$d
	V  <- dec$v
	k  <- 1:s
	U  <- U[,1:s]
	L  <- L[1:s]
	V  <- V[,1:s]
	


	U.sp <- as.spam(U)
	L.sp <- as.spam(L)
	V.sp <- as.spam(V)

	cat("\n")

	cat(paste("Initializing chain", cadeia, sep=" "), "\n")

	cat("\n")


	for(i in 1:s)
	{

		zU <- c(z.sp%*%U.sp[,i])
		zUdiag <- diag.spam(zU)
		zUX <- zUdiag%*%x.sp
		z3 <- as.spam(zUX%*%V.sp[,i])

		z3z <- crossprod.spam(z3)
		z3solve <- solve(z3z)
		z3solvez3 <- tcrossprod.spam(z3solve,z3)	

		L[i] <- z3solvez3%*%part2

		if(i==1){

			xV  <- diag.spam(c(x.sp%*%V.sp[,i]))
			w1 <- xV%*%z.sp
			alpha <- crossprod.spam(w1,part2)

			Hg <- cbind(matrix(1,ngen,1),matrix(0,ngen,ngen-i))
			Hg[1:(ngen-i),(i+1):ncol(Hg)] <- diag(ngen-i)
			x.qr <- qr(Hg)
			Q <- qr.Q(x.qr)
			Q <- Q[,(i+1):ncol(Q)]

			Q <- as.spam(Q)
			alphaQ <- crossprod.spam(alpha, Q)
			AlphaQtQ <- tcrossprod.spam(alphaQ,Q)
			alphafim <- AlphaQtQ%*%alpha

			norm <- sqrt(c(alphafim))

			alphalinha <- crossprod.spam(Q,alpha)/c(norm)


			U.sp[,i] <- Q%*%alphalinha

			#     w2 <- zUX
			zu <- z.sp%*%U.sp[,i]
			zudiag <- diag.spam(c(zu))
			w2 <- zudiag%*%x.sp

			ipso <- crossprod.spam(w2,part2)

			He <- cbind(matrix(0,namb,(s)))
			He[1:s,i:ncol(He)] <- diag(s)
			x.qr <- qr(He)
			Q <- qr.Q(x.qr)
			Q <- as.matrix(Q[,i:ncol(Q)])
			Q <- as.spam(Q)


			norm1 <- crossprod.spam(ipso,Q) 
			norm2 <- tcrossprod.spam(norm1,Q)
			norm <- c(sqrt(norm2%*%ipso))

			ipsolinha <- crossprod.spam(Q,ipso)/norm

			V.sp[,i] <- Q%*%ipsolinha

		}
		if(i>1){

			xv <- x.sp%*%V.sp[,i]
			xvd <- diag.spam(c(xv))
			w1 <- xvd%*%z.sp


			alpha <- crossprod.spam(w1,part2)

			Hg <- cbind(matrix(1,ngen,1),U[,k<i],matrix(0,ngen,ngen-i))
			Hg[1:(ngen-i),(i+1):ncol(Hg)] <- diag(ngen-i)
			x.qr <- qr(Hg)
			Q <- qr.Q(x.qr)
			Q <- as.spam(Q)

			norm1 <- crossprod.spam(alpha,Q) 
			norm2 <- tcrossprod.spam(norm1,Q)
			norm <- c(sqrt(norm2%*%alpha))

			Q <- Q[,(i+1):ncol(Q)]

			qalpha <- crossprod.spam(Q,alpha)
			alphalinha <- qalpha/norm

			U.sp[,i] <- Q%*%alphalinha

			zU <- z.sp%*%U.sp[,i]    
			zUd <- diag.spam(c(zU))
			w2 <- zUd%*%x.sp

			ipso <- crossprod.spam(w2,part2)


			He <- cbind(V[,k<i],matrix(0,namb,(namb-i+1)))
			He[1:(namb-i+1),i:ncol(He)] <- diag(namb-i+1)
			x.qr <- qr(He)
			Q <- qr.Q(x.qr)
			Q <- as.spam(Q)


			norm1 <- crossprod.spam(ipso,Q)
			norm2 <- tcrossprod.spam(norm1,Q)
			norm <- c(sqrt(norm2%*%ipso))

			Q <- Q[,i:ncol(Q)]
			ipsolinha <- crossprod.spam(Q,ipso)/norm

			V.sp[,i] <- Q%*%ipsolinha
		}
	}

	zU <- z.sp%*%U.sp
	xV <- x.sp%*%V.sp
	zuv <- zU*xV
	AMMI <- zuv%*%L

	x1 <- as.spam(x1)
	x1beta <- x1%*%beta
	zg <- z.sp%*%g

	pred <- x1beta + zg + AMMI

	ve   <- c((t(y-pred)%*%(y-pred))/n)
	va   <- c((t(g)%*%g)/ngen)


	niter <- iter*jump + burn


	gen.chain <- matrix(0, nrow=niter, ncol=nlevels(Gen))
	rep.chain <- matrix(0, nrow=niter, ncol=length(beta))
	comp.var <- matrix(0, nrow=niter, ncol=length(c(va,ve)))
	autovU1 <- matrix(0, nrow=niter, ncol=nrow(U))
	autovU2 <- autovU1
	l.list <- matrix(0, nrow=niter, ncol=length(L))
	autoV1 <- matrix(0, nrow=niter, ncol=nrow(V))
	autoV2 <- autoV1
	pred.chain <- matrix(0,nrow=nrow(pred), ncol=niter)

	InvG <- diag.spam(ngen)

	invbeta <- solve(crossprod.spam(x1))
	invbeta <- as.spam(invbeta)

	zz <- crossprod.spam(z.sp)


	invg <- diag.spam(ngen)
	inicio <- proc.time()

  pb <- txtProgressBar(min = 1, max=niter, style=3, char=' \U21D2')

	for(itera in 1:niter)
	{
		ve <- c(ve)
		va <- c(va)

		A1 <- InvG*(ve/va)


		invbx1 <- tcrossprod.spam(invbeta,x1)

		zg <- z.sp%*%g
		izgami <- y-zg-AMMI
		beta1 <- invbx1%*%izgami

		varbeta <- chol(invbeta*ve)

		vbn <- crossprod.spam(varbeta,as.spam(rnorm(nbloc)))
		beta    <- beta1+vbn

		zza <- zz+A1

		invg <- solve(zza)
		invg <- as.spam(invg)

		invgz <- tcrossprod.spam(invg,z.sp)

		x1beta <- x1%*%beta
		yzba <- y-x1beta-AMMI
		g1 <- invgz%*%yzba
		varg <- chol(invg*ve)

		vgn <- crossprod.spam(varg,as.spam(rnorm(ngen)))
		g <- g1+vgn

		zg <- z.sp%*%g
		yxbzg <- y-x1beta-zg


		for (i in 1:s)
		{

			zU <- z.sp%*%U.sp[,i]
			diagzu <- diag.spam(c(zU))
			dzux <- diagzu%*%x.sp
			z3 <- dzux%*%as.spam(V.sp[,i])


			z3z <- crossprod.spam(z3)
			solvez3z <- solve(z3z)
			sz3z <- tcrossprod.spam(solvez3z,z3)

			L1 <- sz3z%*%yxbzg

			varl <- c(sqrt(solvez3z*ve))

			if (i==1){

				L[i] <- rtnorm(1,c(L1),varl, lower=0)

				xv <- x.sp%*%V.sp[,i]
				dxv <- diag.spam(c(xv))
				w1 <- dxv%*%z.sp


				alpha <- crossprod.spam(w1,yxbzg)

				Hg    <- cbind(matrix(1,ngen,1),matrix(0,ngen,ngen-i))
				Hg[1:(ngen-i),(i+1):ncol(Hg)] <- diag(ngen-i)

				x.qr <- qr(Hg)
				Q <- as.matrix(qr.Q(x.qr))
				Q <- as.spam(Q)


				alpha1 <- crossprod(alpha, Q)
				alpha2 <- tcrossprod.spam(alpha1,Q)
				norm <- c(sqrt(alpha2%*%alpha))

				Q <- Q[,(i+1):ncol(Q)]

				alphalinha <- crossprod.spam(Q,alpha)/norm

				mass <-(L[i]*norm)/ve


				autvg <- as.matrix(rmovMF(1,c(mass*alphalinha)))
				class(autvg) <- "matrix"
				autvg <- as.spam(autvg)
				U.sp[,i] <- tcrossprod.spam(Q,autvg)

				zU <- z.sp%*%U.sp[,i]
				dzu <- diag.spam(c(zU))
				w2 <- dzu%*%x.sp

				ipso <- crossprod.spam(w2,yxbzg)

				He   <- cbind(matrix(1,namb,1),matrix(0,namb,(namb-i)))
				He[1:(namb-i),(i+1):ncol(He)] <- diag((namb-i))
				x.qr <- qr(He)
				Q    <- qr.Q(x.qr)
				Q    <- Q[,(i+1):ncol(Q)]
				Q <- as.spam(Q)

				ipsoq <- crossprod.spam(ipso,Q)
				ipqq <- tcrossprod.spam(ipsoq,Q)
				norm <- c(sqrt(ipqq%*%ipso))

				ipsolinha <- crossprod.spam(Q,ipso)/norm
				mass <- c((L[i]*norm)/ve)

				autve<- rmovMF(1,c(mass*ipsolinha))
				class(autve) <- "matrix"
				autve <- as.spam(autve)

				V.sp[,i]<- tcrossprod.spam(Q,autve)
			}

			if (i>1) 
			{
				L[i] <- rtnorm(1,c(L1),c(varl), lower=0, upper=L[i-1])

				xv <- x.sp%*%V.sp[,i]
				diagxv <- diag.spam(c(xv))
				w1 <- diagxv%*%z.sp		

				alpha <- crossprod.spam(w1,yxbzg)

				Hg    <- cbind(matrix(1,ngen,1),U.sp[,k<i],matrix(0,ngen,ngen-i))
				Hg[1:(ngen-i),(i+1):ncol(Hg)] <- diag(ngen-i)
				# 			Hg <- Matrix(Hg, sparse=TRUE)
				x.qr <- qr(Hg)
				Q <- qr.Q(x.qr)
				Q <- Q[,(i+1):ncol(Q)]
				Q <- as.spam(Q)

				alpha1 <- crossprod.spam(alpha, Q)
				alpha2 <- tcrossprod.spam(alpha1,Q)
				norm <- c(sqrt(alpha2%*%alpha))

				alphalinha <- crossprod.spam(Q,alpha)/norm

				mass <- c((L[i]*norm)/ve)

				autvg<- rmovMF(1,c(mass*alphalinha))
				class(autvg) <- "matrix"
				autvg <- as.spam(autvg)

				U.sp[,i]<- tcrossprod.spam(Q,autvg)

				zu <- z.sp%*%U.sp[,i]
				dzu <- diag.spam(c(zu))
				w2 <- dzu%*%x.sp

				ipso <- crossprod.spam(w2,yxbzg)

				He  <- cbind(matrix(1,namb,1),V.sp[,k<i],matrix(0,namb,(namb-i)))
				He[1:(namb-i),(i+1):ncol(He)] <- diag(namb-i)
				x.qr <- qr(He)
				Q    <- qr.Q(x.qr)
				Q    <- Q[,(i+1):ncol(Q)]
				Q <- as.spam(Q)

				ipsoq <- crossprod.spam(ipso,Q)
				ipsoqq <- tcrossprod.spam(ipsoq,Q)
				norm <- c(sqrt(ipsoqq%*%ipso))

				ipsolinha <- crossprod.spam(Q,ipso)/norm

				mass <- c((L[i]*norm)/ve)

				if(i<s){
					autve <- rmovMF(1,c(mass*ipsolinha))
					class(autve) <- "matrix"
					autve <- as.spam(autve)
					V.sp[,i] <- tcrossprod(Q,autve)
				}
				if(i==s){V.sp[,i] <- Q%*%ipsolinha}
			}
		}

		zu <- z.sp%*%U.sp
		xv <- x.sp%*%V.sp
		zuxv <- zu*xv
		AMMI <- zuxv%*%L

		zg <- z.sp%*%g
		z1bzg <- x1beta+zg
		pred <- z1bzg+AMMI

		erro <- t(y-pred)%*%(y-pred)

		ginvg <- crossprod.spam(g,InvG)
		ssg  <- ginvg%*%g

		rve  <- rchisq(1,(n))
		ve   <- (erro/rve)
		rva  <- rchisq(1,(ngen))
		va   <- (ssg/rva)  

#		pb <- txtProgressBar(min = 1, max=niter, style=3)
		setTxtProgressBar(pb, itera)


		comp.var[itera,] <- c(va,ve)
		rep.chain[itera,] <- c(beta)
		gen.chain[itera, ] <- g


		pred.chain[,itera] <- pred


		autovU1[itera,] <- c(U.sp[,1])
		autovU2[itera,] <- c(U.sp[,2])

		l.list[itera,] <- L

		autoV1[itera,] <- c(V.sp[,1])
		autoV2[itera,] <- c(V.sp[,2])

	
	}

	fim <- proc.time()-inicio
	cat("\n")
	cat("Elapsed time:", paste(round(fim[3]/60,3), "minutes", sep=" "), "\n")

	cp.var <- comp.var[-c(1:burn),]
	atu1 <- autovU1[-c(1:burn),]
	atu2 <- autovU2[-c(1:burn),]
	atv1 <- autoV1[-c(1:burn),]
	atv2 <- autoV2[-c(1:burn),]
	ll <- l.list[-c(1:burn),]
	r.chain <- rep.chain[-c(1:burn),]
	gt.chain <- gen.chain[-c(1:burn),]
	pred.chain <- pred.chain[,-c(1:burn)]

	adjust <- seq(1,iter*jump, by=jump)

	cp.var <- cp.var[c(adjust),]
	atu1 <- atu1[c(adjust),]
	atu2 <- atu2[c(adjust),]
	atv1 <- atv1[c(adjust),]
	atv2 <- atv2[c(adjust),]
	ll <- ll[c(adjust),]
	r.chain <- r.chain[c(adjust),]
	gt.chain <- gt.chain[c(adjust),]
	pred.chain <- pred.chain[,c(adjust)]

	colnames(cp.var) <- c("Var.gen","Var.error")
	colnames(atu1) <- levels(Gen)
	colnames(atu2) <- levels(Gen)
	colnames(atv1) <- levels(Env)
	colnames(atv2) <- levels(Env)

	tamenv <- nlevels(Env)-1
  colnames(ll) <- paste("L", 1:tamenv, sep=".")

	colnames(r.chain) <- paste("Rep", levels(Rep), sep=".")
	colnames(gt.chain) <- levels(Gen)

	output <- list(cpvar=cp.var, atu1=atu1,
								 atu2=atu2, atv1=atv1,
								 atv2=atv2, L=ll, rep.chain=r.chain, gen.chain=gt.chain,
								 pred.chain=pred.chain, time.el=fim)
	return(output)
}


