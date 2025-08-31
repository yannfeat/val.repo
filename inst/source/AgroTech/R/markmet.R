#' Spray deposit (metallic marker)
#'
#' @description This is a function to determine spray deposit using metallic markers and then after performing tests of assumptions, analysis of variance and comparison of means
#'
#' @param ppm Concentração
#' @param white White reading
#' @param VL Wah volume (mL)
#' @param AL blade area (cm2)
#' @param trat Vector with treatment
#' @param block Vector with block (if design = "DBC")
#' @param analysis Perform statistical analysis
#' @param design Experiment design
#' @param transf Data transformation
#' @param quali Qualitative or quantitative treatment (\emph{default} is TRUE)
#' @param grau degree of the polynomial (when treatment is quantitative)
#' @param test Parametric or Nonparametric (\emph{default} is "parametric")
#' @param mcomp Mean comparison test (\emph{default} is "tukey")
#' @param ylab y axis name (\emph{default} is expression(mu~cm^2))
#' @param save.xlsx Want to export in excel format (\emph{default} is FALSE)
#'
#' @return Returns the comparison between the treatments of the experiment
#'
#' @author Rodrigo Yudi Palhaci Marubayashi, \email{marubayashi@uel.br}
#' @author Gabriel Danilo Shimizu
#' @author Otavio Jorge Grigoli Abi Saab
#'
#' @references
#' No reference
#'
#' @export
#'
#' @seealso \link{markblue}
#'
#' @examples
#' library(AgroTech)
#' data("example_markmet")
#' with(example_markmet,
#'      markmet(ppm = ppm,
#'             white = 0.02,
#'             VL = 35,
#'             AL = 63.61,
#'             analysis = TRUE,
#'             trat=trat))

markmet=function(ppm,
                 white,
                 VL,
                 AL,
                 analysis=TRUE,
                 trat,
                 block,
                 design="DIC",
                 transf=1,
                 quali=TRUE,
                 grau=1,
                 test="parametric",
                 mcomp="tukey",
                 ylab=expression(mu~cm^2),
                 save.xlsx=FALSE){
  ppmc=(ppm-white)*1000
  microg=(ppmc*VL)/1000
  mcm2=microg/AL
  if(analysis==FALSE){
    respostas=data.frame(conc=ppm,
                         microg,
                         microcm2=mcm2)}
  if(analysis==TRUE){
    respostas=data.frame(conc=ppm,
                         microg,
                         microcm2=mcm2)
    if(design=="DIC"){
      with(respostas,dic.analysis(trat,
                         microcm2,
                         transf = transf,
                         quali=quali,
                         grau = grau,
                         test=test,
                         mcomp=mcomp,
                         ylab=ylab))}
    if(design=="DBC"){
      with(respostas,dbc.analysis(trat,
                         block,
                         microcm2,
                         transf = transf,
                         quali=quali,
                         grau = grau,
                         test=test,
                         mcomp = mcomp,
                         ylab=ylab))}}
  cat("\n")
  if(save.xlsx=="TRUE"){xlsx::write.xlsx(respostas,"result.xlsx")}
  list(response=respostas)
}
