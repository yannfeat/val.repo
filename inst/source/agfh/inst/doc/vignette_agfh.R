## ----setup, include=FALSE-----------------------------------------------------
require(knitr)
require(agfh)
knitr::opts_chunk$set(echo = TRUE,
                      fig.height = 3, fig.align = 'center')

## ----package_options, include=FALSE-------------------------------------------
knitr::opts_knit$set(global.par = TRUE)

user.par <- par(mfrow=c(1,2))

## -----------------------------------------------------------------------------
set.seed(2023)
M <- 50
p <- 3
D <- rep(0.1, M)
lamb <- 1/2
dat <- beta_err_gen(M, p, D, lamb, 1/12, 1/6)

## ---- echo=FALSE--------------------------------------------------------------
par(mfrow=c(1,2))
hist(dat$err, xlab='u', main='Sampling Errors', cex.main=0.75, cex.lab=0.75, cex.axis=0.75)
plot(dat$theta, dat$Y, pch=16, xlab=expression(theta), ylab='Y', main='Response', 
     cex.main=0.75, cex.lab=0.75, cex.axis=0.75)

## -----------------------------------------------------------------------------
n.mcmc <- 1e2
n.thin <- 20
S.init <- seq(-10,10,length.out=M)
thet.var.prior.a <- 1
thet.var.prior.b <- 1
mh.scale.thetas <- 1

## -----------------------------------------------------------------------------
mhg_sampler <- make_agfh_sampler(X=dat$X, Y=dat$Y, D=dat$D,
                                 var_gamma_a=thet.var.prior.a, var_gamma_b=thet.var.prior.b,
                                 S=S.init,
                                 kern.a0=0.1,
                                 kern.a1=0.1,
                                 kern.fuzz=1e-2)
init <- list(beta=rep(0,p),
             theta= predict(lm(dat$Y ~ dat$X), data.frame(dat$X)),
             theta.var=1 ,
             gamma = rep(0, length(S.init)))

mhg.out <- mhg_sampler(init, n.mcmc, n.thin, mh.scale.thetas, trace=FALSE)

## ---- echo=FALSE--------------------------------------------------------------
titles <- c(expression(beta[1]), expression(beta[2]), expression(beta[3]), expression(psi), expression(alpha[0]), expression(alpha[1]))
for (i in 1:3) {
  plot(NA, NA, xlim=c(1,n.mcmc),
       ylim=c(min(mhg.out$param.samples.list$beta[,i]), max(mhg.out$param.samples.list$beta[,i])),
       ylab='', xlab='', main=paste0(titles[i], ' Traceplot'),
       cex.main=0.75, cex.lab=0.75, cex.axis=0.75)
  lines(mhg.out$param.samples.list$beta[,i])
  lines(c(0,1e5), c(dat$Beta[i],dat$Beta[i]), col='red', lwd=2)
}


plot(NA, NA, xlim=c(1,n.mcmc), 
     ylim=c(min(mhg.out$param.samples.list$theta.var),max(mhg.out$param.samples.list$theta.var)),
     ylab='',  xlab='', main='Theta Var Traceplot',
     cex.main=0.75, cex.lab=0.75, cex.axis=0.75)

lines(mhg.out$param.samples.list$theta.var)
lines(c(0,1e5), rep(dat$lambda, 2), col='red', lwd=2)

## ---- echo=FALSE--------------------------------------------------------------
g <- mhg.out$g.final

plot(NA, NA, xlim=c(-2,2),
     ylim=c(min(-3, min(g$gamma)),max(3, max(g$gamma))),
     xlab='u', ylab='g(u)', main='Estimated g Function',
     cex.main=0.75, cex.lab=0.75, cex.axis=0.75)

points(g$S, g$gamma, pch=16)

u_dens <- function(u) {
  (1/sqrt(2*pi))*exp(-u^2/2 + g$g(u))
}

ud.xlim <- c(-2,2)
ud.ylim <- c(0,4)

x.dense <- seq(-12,12,length.out=500)
u.dens <- sapply(x.dense, u_dens)
hist(dat$err, freq = FALSE, xlim=ud.xlim, ylim=ud.ylim, xlab='U', main='Sampling Error Density', breaks = 12, cex.main=0.75, cex.lab=0.75, cex.axis=0.75)
legend('top',legend=c('Empirical Density', 'Estimated Density'), fill=c('gray', 'green'), 
       cex=.75)
lines(x.dense, u.dens, lwd=2, col='green')

## -----------------------------------------------------------------------------
params.init <- list(beta=rep(0,p),
                    theta=predict(lm(dat$Y ~ dat$X), data.frame(dat$X)),#rep(0,M),
                    theta.var=1)
sampler <- make_gibbs_sampler(dat$X, dat$Y, dat$D, thet.var.prior.a, thet.var.prior.b)
gibbs.out <- sampler(params.init, n.mcmc, n.thin)

## ---- echo=FALSE--------------------------------------------------------------
for (i in 1:3) {
  plot(NA, NA, xlim=c(1,n.mcmc),
       ylim=c(min(gibbs.out$param.samples.list$beta[,i]), max(gibbs.out$param.samples.list$beta[,i])),
       ylab='', xlab='', main=paste0(titles[i], ' Traceplot'),
       cex.main=0.75, cex.lab=0.75, cex.axis=0.75)
  lines(gibbs.out$param.samples.list$beta[,i])
  lines(c(0,1e5), c(dat$Beta[i],dat$Beta[i]), col='red', lwd=2)
}


plot(NA, NA, xlim=c(1,n.mcmc), 
     ylim=c(min(gibbs.out$param.samples.list$theta.var),max(gibbs.out$param.samples.list$theta.var)),
     ylab='',  xlab='', main='Theta Var Traceplot',
     cex.main=0.75, cex.lab=0.75, cex.axis=0.75)

lines(gibbs.out$param.samples.list$theta.var)
lines(c(0,1e5), rep(dat$lambda, 2), col='red', lwd=2)

## -----------------------------------------------------------------------------
theta.ests.freqeb <- RM_theta_eblup(dat$X, dat$Y, dat$D)

adj_reml_thvar <- adj_resid_likelihood_theta_var_maker(dat$X, dat$Y, dat$D)
theta.var.est.adj.reml <- theta_var_est_grid(adj_reml_thvar)
theta.ests.adj.reml <- RM_theta_eblup(dat$X, dat$Y, dat$D, theta.var.est.adj.reml)

theta.ests.lm <- predict(lm(dat$Y ~ dat$X), data.frame(dat$X))

## ---- echo=FALSE, fig.height = 4, fig.width=4---------------------------------
par(mfrow=c(1,1))
n.mcmc <- dim(mhg.out$param.samples.list$theta)[1]
n.sample.map.est <- 100

M <- dim(dat$X)[1]
theta.maps.agfh <- sapply(1:M, function(m) {map_from_density(mhg.out$param.samples.list$theta[(n.mcmc-n.sample.map.est):n.mcmc,m])})
theta.maps.gibbs <- sapply(1:M, function(m) {map_from_density(gibbs.out$param.samples.list$theta[,m])})


plot(dat$theta, theta.ests.lm, pch=16, col='blue',
     xlab=expression(theta[true]), ylab=expression(theta[est]),
     main='Theta Estimates',
     cex.main=0.75, cex.lab=0.75, cex.axis=0.75)
lines(c(-100,100), c(-100,100))
points(dat$theta, theta.ests.freqeb, pch=16, col='orange')
points(dat$theta, theta.ests.adj.reml, pch=16, col='magenta')
points(dat$theta, theta.maps.gibbs, pch=16, col='gray')
points(dat$theta, theta.maps.agfh, pch=16, col='green')
legend('topleft', legend=c('AGFH MAP', 'FH HB MAP', 'LM', 'Freq/EB', 'Adj REML'), col=c('green', 'gray', 'blue', 'orange', 'magenta'), pch=16, cex=0.75)

## ---- echo=FALSE--------------------------------------------------------------
mses <- data.frame(
  AGFH   =mse(dat$theta, theta.maps.agfh),
  HB   =mse(dat$theta, theta.maps.gibbs),
  LM     =mse(dat$theta, theta.ests.lm),
  FreqEB =mse(dat$theta, theta.ests.freqeb)
)
kable(round(mses,2), caption='MSEs For Each Method')

## ---- echo=FALSE--------------------------------------------------------------
# reset user's par 
par(user.par)

