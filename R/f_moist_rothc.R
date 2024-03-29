#' Effects of moisture on decomposition rates according to the RothC model
#'
#' Calculates the effects of moisture (precipitation and pan evaporation) on
#' decomposition rates according to the RothC model.
#' @name f_moist_rothc
#' @title Effect of moisture on C flows
#' @param pp A vector with monthly precipitation (mm).
#' @param et A vector with same length with open pan evaporation or
#' evapotranspiration (mm).
#' @param s_thick Soil thickness in cm. Default for Rothamsted is 23 cm.
#' @param pclay Percent clay.
#' @param pE Evaporation coefficient. If open pan evaporation is used pE=0.75.
#' If Potential evaporation is used, pE=1.0.
#' @param soil_cover Logical vector with months where the soil is covered. If only a single value is passed the whole period will be treated homogeneously.
#' @description Extended version of \code{\link[SoilR]{fW.RothC}} coping with different soil cover regimes.
#' @return A vector with the rate modifying factor.
#' @author Marcos Alves
#' @import SoilR
#' @export
#'

f_moist_rothc <- function(pp, et, s_thick, pclay, pE = 1.0, soil_cover = TRUE) {
  if (length(et) != length(pp)) {
    stop("pp and et must have the same lengh")
  }
  soil_cover <- !as.logical(soil_cover)
  if (length(soil_cover) > 1) {
    if (length(soil_cover) != length(pp) | length(soil_cover) != length(et)) {
      stop("pp, et and soil_cover must have the same lengh")
    }
    fwBare <- fW.RothC(
      P = pp, E = et,
      S.Thick = s_thick, pClay = pclay,
      pE = pE, bare = TRUE
    )$b
    fwCoverd <- fW.RothC(
      P = pp, E = et,
      S.Thick = s_thick, pClay = pclay,
      pE = pE, bare = FALSE
    )$b
    fw <- fwBare
    fw[!soil_cover] <- fwCoverd[!soil_cover]
  } else {
    fw <- fW.RothC(
      P = pp, E = et,
      S.Thick = s_thick, pClay = pclay,
      pE = pE, bare = soil_cover
    )$b
  }
  return(fw)
}


f_moist_rothc2 <- function(pp, et, s_thick, pclay, pE = 1.0, soil_cover = TRUE) {
  if (length(et) != length(pp)) {
    stop("pp and et must have the same lengh")
  }
  soil_cover <- !as.logical(soil_cover)
  if (length(soil_cover) > 1) {
    if (length(soil_cover) != length(pp) | length(soil_cover) != length(et)) {
      stop("pp, et and soil_cover must have the same lengh")
    }
    fwBare <- fW.RothC(
      P = pp, E = et,
      S.Thick = s_thick, pClay = pclay,
      pE = pE, bare = TRUE
    )$b
    fwCoverd <- fW.RothC(
      P = pp, E = et,
      S.Thick = s_thick, pClay = pclay,
      pE = pE, bare = FALSE
    )$b
    fw <- fwBare
    fw[!soil_cover] <- fwCoverd[!soil_cover]
  } else {
    fw <- fW.RothC(
      P = pp, E = et,
      S.Thick = s_thick, pClay = pclay,
      pE = pE, bare = soil_cover
    )$b
  }
  return(fw)
}



fwBare <- fW.RothC2(
  P = c(1, 2, 3), E = c(1, 2, 3),
  S.Thick = 1, pClay = 1,
  pE = 1, bare = c(1, 1, 0)
)$b
