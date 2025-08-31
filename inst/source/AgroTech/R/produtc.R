#' Amount of phytosanitary product per spray tank
#'
#' @description This is a function to determine the amount of commercial product to be placed in the sprayer tank at each fill
#'
#' @param Ct Spray tank volumetric capacity (L)
#' @param Dose Product dose to be applied (L/ha, mL/ha, kg/ha, g/ha)
#' @param Q Application rate (L/ha)
#'
#' @return Returns values for amount of product (L or kg)
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
#' \link{flowrat}
#'
#' @examples
#' product(Ct = 800,Dose = 200,Q = 100)

product=function(Ct,Dose,Q){
  product=(Ct*Dose)/Q
  message(black(paste("Amount of product (L or kg) to be added to the tank at each sprayer refill: ",product)))
}
