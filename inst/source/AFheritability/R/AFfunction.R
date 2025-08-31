############## AF as a function of heritability #####################
#' @title Plot the attributable fraction as a function of heritability, disease prevalence, size of target group and intervention effect.
#' @description \code{AFfunction} is a function which illustrates the AF as a function of heritability, disease prevalence, size of target group and intervention effect.
#' @param Prevalence an estimate of the disease prevalence
#' @param Heritability an estimate of the disease heritability
#' @param Target proportion of those at highest genetic risk being targeted by the intervention
#' @param Intervention effect of intervention
#' @param xaxis option to specify which of the arguments \code{Prevalence, Heritability, Target} or \code{Intervention} should be used as the xaxis of the plot. The argument \code{xaxis} is a string with values \code{"Prevalence", "Heritability", "Target"} or \code{"Intervention"}.
#' @param compare option to specify which of the arguments \code{Prevalence, Heritability, Target} or \code{Intervention} should be used for comparisons. The argument \code{compare} can be specified as a numeric vector with a range of values or as a single value, see examples.
#' @param Intervention_type an option to specify how the intervention is expected to affect the genetic liability distribution. The default option \code{"location"} assumes that the intervention shifts the genetic liability distribution to lower levels, among those targeted by the intervention. The option \code{"scale"} assumes that the intervention reduce the variance of the genetic liability distribution, among those targeted by the intervention.
#' @param plot option to return a plot. Default is set to \code{TRUE}.
#' @param legend option to return a legend in the plot. Default is set to \code{TRUE}.
#' @param cex specifies the text size in the plot. Default is set to size \code{1.4}.
#' @param ... further arguments to be passed to the ggplot function. See \code{\link[ggplot2]{ggplot}}.
#' @return \item{AF}{the AF as a function of heritability, disease prevalence, size of target group and intervention effect.}
#' @return \item{plot}{Plot of the AF as a function of either heritability, disease prevalence, size of target group and intervention effect. The legend shows a comparison variable.}
#' @details The AFfunction() is a function that produce a plot of the AF as a function of \code{Prevalence, Heritability, Target} or \code{Intervention}. A user interface of the function is provided in \code{\link[AFheritability]{runShinyApp}}.
#' @references Dahlqwist E et al. (2019) <doi:10.1007/s00439-019-02006-8>.
#' @examples
#'# Example
#' heritability <- seq(0,1, by=0.1)
#' target_sizes <- sort(c(0.30, 0.25, 0.20, 0.15, 0.05, 0.01))
#'
#' AF_h <- AFfunction(Prevalence=0.5, Heritability = heritability,
#'                    Target = target_sizes, Intervention = 1,
#'                    compare="Target", xaxis = "Heritability",
#'                    ylim = c(0,0.3), cex = 1.6)
#'
#' AF_h
#' @importFrom ggplot2 ggplot aes geom_line geom_point theme_bw theme element_blank element_line element_text labs
#' @importFrom reshape2 melt
#' @importFrom stats qnorm
#' @import stats mvtnorm ggplot2 reshape2
#' @export
AFfunction <- function(Prevalence, Heritability, Target, Intervention, xaxis, compare, Intervention_type="location", plot = TRUE, legend = TRUE, cex=1.4,...){

  if (requireNamespace("ggplot2", quietly = TRUE)) {
    requireNamespace("ggplot2")
  } else {
    plot == FALSE
  }

  if(compare == 'Prevalence' & xaxis == 'Heritability') func <- AF_heritability_prevalence
  if(compare == 'Target' & xaxis =='Heritability' ) func <- AF_heritability_target
  if(compare == 'Intervention' & xaxis == 'Heritability') func <- AF_heritability_intervention

  if(compare =='Heritability' & xaxis == 'Prevalence') func <- AF_prevalence_heritability
  if(compare =='Target' & xaxis == 'Prevalence') func <- AF_prevalence_target
  if(compare == 'Intervention' & xaxis == 'Prevalence') func <- AF_prevalence_intervention

  if(compare == 'Heritability' & xaxis =='Target') func <- AF_target_heritability
  if(compare =='Prevalence' & xaxis =='Target') func <- AF_target_prevalence
  if(compare == 'Intervention' & xaxis =='Target' ) func <- AF_target_intervention

  if(compare =='Heritability' & xaxis == 'Intervention') func <- AF_intervention_heritability
  if(compare =='Prevalence' & xaxis =='Intervention') func <- AF_intervention_prevalence
  if(compare =='Target' & xaxis == 'Intervention') func <- AF_intervention_target

  AF_est <- func(Prevalence, Heritability, Target, Intervention, Intervention_type, plot = TRUE, legend = TRUE, ...)
  return(AF_est)
}




