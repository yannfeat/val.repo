#' Flow calculation as a function of working pressure
#'
#' @description This is a function to determine the flow rate of a spray nozzle as a function of the working pressure
#'
#' @param q1 Nozzle flow 1 (L/min)
#' @param q2 Nozzle flow 2 (L/min)
#' @param p1 Nozzle pressure 1 (bar)
#' @param p2 Nozzle pressure 2 (bar)
#'
#' @return Returns values of flow (L/min) or pressure (bar)
#'
#' @details
#' Nozzle flow 1:
#' \deqn{q1=\frac{\sqrt{p1}}{\sqrt{p2}}*q2}
#' Nozzle flow 2:
#' \deqn{q2=\frac{q1}{\frac{\sqrt{p1}}{\sqrt{p2}}}}
#' Nozzle pressure 1:
#' \deqn{p1=(\sqrt{p2}*\frac{q1}{q2})^2}
#' Nozzle pressure 2:
#' \deqn{p2=(\frac{\sqrt{p1}}{(\frac{q1}{q2})})^2}
#'
#' @author Rodrigo Yudi Palhaci Marubayashi, \email{marubayashi@uel.br}
#' @author Gabriel Danilo Shimizu
#' @author Otavio Jorge Grigoli Abi Saab
#'
#' @references
#' ANDEF Associacao Nacional de Defesa Vegetal. Manual de tecnologia de aplicacao de produtos fitossanitarios. Campinas: Linea Creativa, 2004. 50p.
#'
#' BOLLER, W.; RAETANO, C. G. Bicos e pontas de pulverizacao de energia hidraulica, regulagens e calibracao de pulverizadores de barras. In: ANTUNIASSI, U. R.; BOLLER, W. (Organizadores). Tecnologia de aplicacao para culturas anuais. Passo Fundo: Aldeia Norte; Botucatu: FEPAF, 2011. p.51-82.
#'
#' SPRAYING SYSTEMS CO. Catalogo 51A-PT - Produtos de pulverizacao para agricultura. Wheaton: Spraying Systems Co., 2014. 160p.
#'
#' @export
#'
#' @seealso \link{flowrat}
#' \link{product}
#'
#' @examples
#' flowpres(q1=NA,q2=0.80,p1=1.00,p2=2.80)
#' flowpres(q1=0.48,q2=0.80,p1=1.00,p2=NA)

flowpres=function(q1,q2,p1,p2){
  requireNamespace("crayon")
  if(is.na(q1)==TRUE){q1=(sqrt(p1)/sqrt(p2))*q2
  message(black("Nozzle flow 1 (L/min): "))
  message(round(q1,2))}
  if(is.na(q2)==TRUE){q2=q1/(sqrt(p1)/sqrt(p2))
  message(black("Nozzle flow 2 (L/min): "))
  message(round(q2,2))}
  if(is.na(p1)==TRUE){p1=(sqrt(p2)*(q1/q2))^2
  message(black("Nozzle pressure 1 (bar): "))
  message(round(p1,2))}
  if(is.na(p2)==TRUE){p2=(sqrt(p1)/(q1/q2))^2
  message(black("Nozzle pressure 2 (bar): "))
  message(round(p2,2))}
}
