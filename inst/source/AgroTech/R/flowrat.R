#' Calculation of required spray nozzle flow
#'
#' @description This is a function to determine the required flow rate of a spray nozzle
#'
#' @param q Nozzle flow (L/min)
#' @param Q Application rate (L/ha)
#' @param V Sprayer speed (km/h)
#' @param W Spacing between spray nozzles (cm)
#'
#' @note 60000 Units conversion factor
#'
#' @return Returns values for flow, application rate, sprayer speed, spacing between spray tips
#'
#' @details
#' Application rate (L/ha):
#' \deqn{Q=\frac{60000*q}{V*W}}
#' Nozzle flow (L/min):
#' \deqn{q=\frac{Q*(V*W)}{60000}}
#' Sprayer speed (km/h):
#' \deqn{V=\frac{\frac{60000*q}{Q}}{W}}
#' Spacing between spray nozzles (m):
#' \deqn{W=\frac{\frac{60000*q}{Q}}{V}}
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
#' @seealso \link{flowpres}
#' \link{product}
#'
#' @examples
#' flowrat(Q = 190,q = NA,V = 10,W = 50)

flowrat=function(Q,q,V,W=50){
  if(is.na(Q)==TRUE){Q=(60000*q)/(V*W)
  message(black("Taxa de aplicacao (L/ha): "))
  message(round(Q,2))}
  if(is.na(q)==TRUE){q=(Q*(V*W))/60000
  message(black("Vazao da ponta (L/min): "))
  message(round(q,2))}
  if(is.na(V)==TRUE){V=((60000*q)/Q)/W
  message(black("Velocidade do Pulverizador (km/h): "))
  message(round(V,2))}
  if(is.na(W)==TRUE){W=((60000*q)/Q)/V
  message(black("Espacamento entre pontas (m): "))
  message(round(W,2))}
}
