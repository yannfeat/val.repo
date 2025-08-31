# Solve anomalous diffusion using FDM
# Author: Jader Lugon Junior
# Date: 02/2019
# Version: 0.1.19
#
# AdvDif4(parm,func)
# parm = alternative to inform parameters data
# func = alternative to inform functions
#
# Data description
# k2 = 2nd order diffusion coefficient parameter
# k4 = 4th order diffusion coefficient parameter
# v = velocity
# l = dominium
# m = space discretization step number
# tf = final time of simulation
# n = time discretization step number
# w10,w11,w12,w20,w21,w22 = left values coefficients to define boundary conditions
# e10,e11,e12,e20,e21,e22 = right values coefficients to define boundary conditions
#
# Functions description
# fbeta = beta diffusion fraction function
# dbetadp = beta derivative function
# fn = initial condition function
# fs = source function
# fw1,fw2 = left border function for boundary conditions
# fe1,fe2 = right border function for boundary conditions

#
#
#
#' @export
AdvDif4 <- function(parm=NA,func=NA)
{
  # Anomalous diffusion function and variables
  if (any((!is.na(parm)),(!is.na(func))))
  {k2 <- parm[1]
  k4 <- parm[2]
  v <- parm[3]
  l <- parm[4]
  m <- parm[5]
  tf <- parm[6]
  n <- parm[7]
  w10 <- parm[8]
  w11 <- parm[9]
  w12 <- parm[10]
  w20 <- parm[11]
  w21 <- parm[12]
  w22 <- parm[13]
  e10 <- parm[14]
  e11 <- parm[15]
  e12 <- parm[16]
  e20 <- parm[17]
  e21 <- parm[18]
  e22 <- parm[19]
  fbeta <- func$fbeta
  dbetadp <- func$dbetadp
  fn <- func$fn
  fs <- func$fs
  fw1 <- func$fw1
  fw2 <- func$fw2
  fe1 <- func$fe1
  fe2 <- func$fe2}
  if (any((!is.function(fbeta)),(!is.function(dbetadp)),(!is.function(fn)),(!is.numeric(k2)),(!is.numeric(k4)),(!is.numeric(v)),(!is.numeric(l)),(!is.numeric(m)),(!is.numeric(tf)),(!is.numeric(n)),(!is.numeric(w10)),(!is.numeric(w11)),(!is.numeric(w12)),(!is.numeric(w20)),(!is.numeric(w21)),(!is.numeric(w22)),(!is.function(fw1)),(!is.function(fw2)),(!is.numeric(e10)),(!is.numeric(e11)),(!is.numeric(e12)),(!is.numeric(e20)),(!is.numeric(e21)),(!is.numeric(e22)),(!is.function(fe1)),(!is.function(fe2))))
   {stop('Advection Bi-Flux Difusive Problem functions or variables are wrong or missing.')}
  dx <- l/m
  dt <- tf/n
  p1 <- seq(from=dx, to=l-dx, by=dx)
  bet <- p1
  aw <- seq(from=dx, to=l-2*dx, by=dx)
  aww <- seq(from=dx, to=l-3*dx, by=dx)
  p <- matrix(nrow=n,ncol=m+1)
  ap <- p1
  ae <- aw
  aee <- aww
  b <- p1
  #
  # Initial condition definition
  #
  j <- 1
  while(j<=m)
  {p1[j] <- fn(dx*j)
  j <- j+1}
  #
  # Defining some useful values
  #
  auxi6 <- 12*dx^4
  #
  # Temporal loop
  #
  t1 <- dt
  i <- 1
  while(i<=n)
  {
    #
    # Boundary conditions
    #
    if (all((w12==0),(w22==0)))
      # left border second derivative is unknown
      #
    {bc1 <- 1
    cw <- ((w21*fw1(t1))-(w11*fw2(t1)))/((w10*w21)-(w11*w20))
    dfw1 <- ((w20*fw1(t1))-(w10*fw2(t1)))/((w20*w11)-(w21*w10))
    }
    if (all((w11==0),(w21==0)))
      # left border first derivative is unknown
      #
    {bc1 <- 2
    cw <- ((w22*fw1(t1))-(w12*fw2(t1)))/((w10*w22)-(w12*w20))
    dfw2 <- ((w20*fw1(t1))-(w10*fw2(t1)))/((w12*w20)-(w22*w10))
    }
    if (all((w10==0),(w20==0)))
      # left border function value is unknown
      #
    {bc1 <- 3
    dfw1 <- ((w22*fw1(t1))-(w12*fw2(t1)))/((w11*w22)-(w12*w21))
    dfw2 <- ((w21*fw1(t1))-(w11*fw2(t1)))/((w12*w21)-(w11*w22))
    }
    if (all((e12==0),(e22==0)))
      # right border second derivative is unknown
      #
    {bc2 <- 1
    ce <- ((e21*fe1(t1))-(e11*fe2(t1)))/((e10*e21)-(e11*e20))
    dfe1 <- ((e20*fe1(t1))-(e10*fe2(t1)))/((e20*e11)-(e21*e10))
    }
    if (all((e11==0),(e21==0)))
      # right border first derivative is unknown
      #
    {bc2 <- 2
    ce <- ((e22*fe1(t1))-(e12*fe2(t1)))/((e10*e22)-(e12*e20))
    dfe2 <- ((e20*fe1(t1))-(e10*fe2(t1)))/((e12*e20)-(e22*e10))
    }
    if (all((e10==0),(e20==0)))
    {bc2 <- 3
    # right border function value is unknown
    #
    dfe1 <- ((e22*fe1(t1))-(e12*fe2(t1)))/((e11*e22)-(e12*e21))
    dfe2 <- ((e21*fe1(t1))-(e11*fe2(t1)))/((e12*e21)-(e11*e22))
    }
    #
    # Building Matrix "A" and vector "b"
    #
    j <- 1
    while(j<=m-1)
    {
      bet[j] <- fbeta(p1[j])
      k22 <- bet[j]*k2
      k44 <- (-bet[j]*(1-bet[j])*k4)
      if (all((j>3),(j<m-3)))
       {dbedx <- dbetadp(bet[j])*(p1[j-2]-8*p1[j-1]+8*p1[j+1]-p1[j+2])/12/dx}
       else {dbedx <- 0}
      al1 <- k2*dbedx
      al2 <- k4*(1-2*bet[j])*dbedx
      auxi1 <- 12*dx^4+30*k22*dx^2*dt-6*12*k44*dt
      auxi2 <- (-16*k22*dx^2*dt+4*12*k44*dt+8*v*dx^3*dt-8*al1*dx^3*dt+12*al2*dx*dt)
      auxi3 <- (-16*k22*dx^2*dt+4*12*k44*dt-8*v*dx^3*dt+8*al1*dx^3*dt-12*al2*dx*dt)
      auxi4 <- k22*dx^2*dt-12*k44*dt-v*dx^3*dt+al1*dx^3*dt-6*al2*dx*dt
      auxi5 <- k22*dx^2*dt-12*k44*dt+v*dx^3*dt-al1*dx^3*dt+6*al2*dx*dt
      ap[j] <- auxi1
      b[j] <- auxi6*(p1[j]+dt*fs(dx*j,i*dt))
      if (j==1)
      {if (bc1==1)
        {b[j] <- b[j]+2*dx*dfw1*auxi5-auxi3*cw
        ap[j] <- auxi1+auxi5}
      if (bc1==2)
        {b[j] <- b[j]-2*auxi5*cw-dx^2*auxi5*dfw2-auxi3*cw
        ap[j] <- auxi1-auxi5}
      if (bc1==3)
        {b[j] <- b[j]+2*auxi5*dx*dfw1+auxi3*(dx*dfw1+dx^2*dfw2/2)
        ap[j] <- auxi1+auxi5+auxi3}
      }
      if (j==2)
      {if (bc1==1)
        {b[j] <- b[j]-auxi5*cw}
      if (bc1==2)
        {b[j] <- b[j]-auxi5*cw}
      if (bc1==3)
        {b[j] <- b[j]+auxi5*(dx*dfw1+dx^2*dfw2/2)
        ap[j] <- auxi1+auxi5}
      }
      if (j==m-1)
      {if (bc2==1)
        {b[j] <- b[j]-2*dx*dfe1*auxi4-auxi2*ce
        ap[j] <- auxi1+auxi4}
      if (bc2==2)
        {b[j] <- b[j]-2*auxi4*ce-dx^2*auxi4*dfe2-auxi2*ce
        ap[j] <- auxi1-auxi4}
      if (bc2==3)
        {b[j] <- b[j]-2*auxi4*dx*dfe1+auxi2*(dx^2*dfe2/2-dx*dfe1)
        ap[j] <- auxi1+auxi4+auxi2}
      }
      if (j==m-2)
      {if (bc2==1)
        {b[j] <- b[j]-auxi4*ce}
      if (bc2==2)
        {b[j] <- b[j]-auxi4*ce}
      if (bc2==3)
        {b[j] <- b[j]+auxi4*(dx^2*dfe2/2-dx*dfe1)
        ap[j] <- auxi1+auxi4}
      }
      if (j<m-2)
      {aee[j] <- auxi4}
      if (j<m-1)
      {ae[j] <- auxi2}
      if (j>1)
      {aw[j-1] <- auxi3}
      if (j>2)
      {aww[j-2] <- auxi5}
      j <- j+1}
    #
    # saving previous result
    #
    if (bc1==3)
     {cw <- p1[1]-dx*dfw1-dx^2*dfw2/2}
    p[i,1] <- cw
    j <- 2
    while(j<=m)
     {p[i,j] <- p1[j-1]
     j <- j+1}
    if (bc2==3)
     {ce <- p1[m]+dx*dfe1-dx^2*dfe2/2}
    p[i,m+1] <- ce
    #
    # solving the system "Ax=b"
    #
    p1 <- pentaSolve(aww,aw,ap,ae,aee,b)
    # Matrix alternative to pentaSolve
    #a <- bandSparse(m-1,k=c(-2,-1,0,1,2),diag=list(aww,aw,ap,ae,aee))
    #p1 <- solve(a,b,sparse=TRUE)
    t1 <- t1+dt
    i <- i+1}
    #write.table(p, file = "output.txt", append = FALSE, quote = FALSE, sep = " ",
    #        eol = "\n", na = "NA", dec = ".", row.names = FALSE,
    #        col.names = FALSE, qmethod = c("escape", "double"),
    #        fileEncoding = "")
  return(p)
}
