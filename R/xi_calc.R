#' @title AgRothC
#' @description Process and run RothC.
#' @name xi_calc
#' @param env Environmental pre processed data
#' @param mgmt management preprocessed data
#' @param cf5 calibration factor Evapotranspiration coeff
#' @author Marcos Alves
#' @export
#' @import SoilR rlang

# env <- environmental_variables(50, 9, 30)

xi_calc <- function(env, mgmt, cf5 = 1) {
  # flow rate effects distribution (baseline)

  temp <- rep_along(mgmt$t, env[, "TS_AV"])
  temp[temp < -18.3] <- -18.3 # function not defined for places with average temperature bellow 18.3 ºC
  prec <- rep_along(mgmt$t, env[, "PRECTOTCORR_AV"])
  evap <- rep_along(mgmt$t, env[, "EVPTRNS_AV"])

  fc <- fC_crop_retainment(mgmt$cover)
  ft <- fT.RothC(temp)
  fw <- fW.RothC2(
    P = prec,
    E = evap,
    S.Thick = 30,
    pClay = 23.4,
    pE = 1 * cf5,
    bare = mgmt$cover
  )

  xi <- as.vector(fw * ft * fc)$b * mgmt$tillage_cr
  xi <- data.frame(mgmt$t, xi)
  return(xi)
}
