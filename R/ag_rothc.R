#' @title Agrenna RothC full implementation
#' @description Process and run RothC.
#' @param x google sheets input
#' @param cf1 Slope yield inputs
#' @param cf2 Intercept yield inputs
#' @param cf3 DR ratio
#' @param cf4 Decomposition rate
#' @param cf5 Evapotranspiration coeffj
#' @param write write output of management, only used for debugging!
#' @name ag_rothC
#' @author Marcos Alves
#' @export
#' @import SoilR

# cf1 = Slope yield inputs
# cf2 = Intercept yield inputs
# cf3 = DR ratio
# cf4 = Decomposition rate
# cf5 = Evapotranspiration coeffj

ag_rothC <- function(x, write = F, cf1 = 1, cf2 = 1, cf3 = 1, cf4 = 1, cf5 = 1, cf6 = 1) {
  mgmt <- management_variables(x,x$Type_of_field[1], cf1 = cf1, cf2 = cf2, cf3 = cf3, write = write)
  env <- environmental_variables(x$Latitude,x$Longitude, x$Sampling_depth_increment)
  xi <- xi_calc(env, mgmt, cf5 = cf5)
  if(!is.numeric(x$SOC_Start_Converted) | x$SOC_Start_Converted == 0) {
    ini <- ini_carbon_pools(env$Carbon[1], env, xi, cf4)
  } else {
    ini <- ini_carbon_pools(x$SOC_Start_Converted, env, xi, cf4)
  }

  model <- RothCModel2(
    t = mgmt$t,
    ks = c(k.DPM = 10 * cf4, k.RPM = 0.3 * cf4, k.BIO = 0.66 * cf4, k.HUM = 0.02 * cf4, k.IOM = 0),
    C0 = ini$C0,
    In = data.frame(mgmt$t,(mgmt$residues + ini$In0) * cf6),
    FYM = data.frame(mgmt$t,mgmt$fym),
    DR =  data.frame(mgmt$t,mgmt$DR * mgmt$cf3),
    clay = env$ParticleSizeClay[1],
    xi = xi,
  )
 C <- getC(model)
 return(C)
}
