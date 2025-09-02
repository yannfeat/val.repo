## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo=FALSE, warning=FALSE, message=FALSE, out.width='100%'
)

## ----plothcr, fig.cap="A threshold catch shape HCR. The shape is defined by four parameters: Cmin and Cmax (which determine the minimum and maximum catch limit) and Blim and Belbow (which determine the start and stop of sloping section).", fig.width=8, fig.height=6----
  xrange <- c(0, 1)
  yrange <- c(0, 150)
  xlab <- "Estimated stock biomass"
  ylab <- "Next catch limit (t)"
  # Plot empty axes 
  plot(x=xrange,y=yrange,type="n",xlab=xlab, ylab=ylab, xaxs="i", yaxs="i") 
  lines(x = c(0, 0.2, 0.5, 1),
        y = c(rep(10, 2), rep(140, 2)), lwd=2, lty=1, col="red")
  grid()
  # Label the parameters
  text(x=0.3, y=10, labels = "Cmin", pos=4, col="blue")
  lines(x=c(0.2, 0.3), y=c(10, 10), lty=2, col="blue")
  text(x=0.4, y=140, labels = "Cmax", pos=2, col="blue")
  lines(x=c(0.4, 0.5), y=c(140, 140), lty=2, col="blue")
  text(x=0.2, y=50, labels = "Blim", pos=3, col="blue")
  lines(x=c(0.2, 0.2), y=c(10, 50), lty=2, col="blue")
  text(x=0.5, y=110, labels = "Belbow", pos=1, col="blue")
  lines(x=c(0.5, 0.5), y=c(110, 140), lty=2, col="blue")

## ----setup, echo=TRUE, eval=FALSE---------------------------------------------
#  library(AMPLE)
#  intro_hcr()

## ----start, fig.cap="The opening screen of the 'Introduction to HCRs' app."----
knitr::include_graphics("intro_hcr_start.png")

## ----advance, fig.cap="After pressing Advance a single time, fishing has occurred in 2020 using the catch limit that was set by the HCR. The stock biomass has been affected by the new catch limit. The HCR uses the estimate of stock biomass at the start of 2021 to set the catch limit for 2021."----
knitr::include_graphics("intro_hcr_hcr11.png")

## ----hcr1, fig.cap="The results of running a full projection of HCR 1."-------
knitr::include_graphics("intro_hcr_hcr1.png")

## ----hcr2, fig.cap="The results of running a full projection with HCR 2."-----
knitr::include_graphics("intro_hcr_hcr2.png")

## ----hcr3, fig.cap="The results of running a full projection with HCR 3."-----
knitr::include_graphics("intro_hcr_hcr3.png")

## ----hcr1noise, fig.cap="The results of running a full projection of HCR 1 with biological variability (note that your plot may look different)."----
knitr::include_graphics("intro_hcr_hcr1_noise.png")

