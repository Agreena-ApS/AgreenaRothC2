#' @title Initialize soil carbon
#' @description Find the equilibrium initial soil carbon for all rothc carbon pools.
#' @param ini_carbon initial C value
#' @param env_clay caly content
#' @param xi decomposition rate factors
#' @param cf4 Calbration factors for Decomposition rates
#' @name ini_carbon_pools
#' @author Marcos Alves
#' @export
#' @import SoilR


ini_carbon_pools <- function(ini_carbon, env_clay,xi, cf4) {
  iom <- 0.049 * (ini_carbon^(1.139))
  soilC_calib <- function(inp_calib) {
    c_init <<- CeqlRoth(
      c(k.DPM = 10 * cf4, k.RPM = 0.3 * cf4, k.BIO = 0.66 * cf4, k.HUM = 0.02 * cf4, k.IOM = 0),
      C0 = c(DPM = 0, RPM = 0, BIO = 0, HUM = 0, IOM = iom),
      In = inp_calib,
      clay = mean(env_clay$ParticleSizeClay[1:3]),
      xi = xi
    )
    soilC <- sum(c_init)
    return((sum(soilC) - ini_carbon)^2)
  }
  inp_calib <- optimize(f = soilC_calib, c(0, 50))$minimum
  pool_init <- as.vector(c_init)
  return(list("C0" = pool_init, "In0" = inp_calib))
}
