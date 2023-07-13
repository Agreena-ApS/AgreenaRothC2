#' @title Yield to residues
#' @description Calculate crop specific biomass residues
#' baseline and scenario runs.
#' @name yield_to_resid2
#' @param yield vector with crop yields
#' @param cropname vector with cropnames
#' @param cf1 Coefficient factor for slope yield inputs
#' @param cf2 Coefficient factor Intercept yield inputs
#' @return vector with crop residues in t C/ha
#' @author Marcos Alves
#' @export

yield_to_resid2 <- function(yield, cropname, cf1 = 1, cf2 = 1) {
  # cfg <- CFGs[cropname, 1]
  cropname <- sapply(cropname,trimws)
  yld2bio <- load_yld2bio()
  coefs <- yld2bio[cropname, ]
  bio_inputs <-
    (as.numeric(yield) * coefs[, "Dry.matter.fraction.FDM"] * coefs[, "Slope.a"] * cf1 + coefs[, "Intercept.b"] * cf2) * 0.15 # CFT considers a fixed amount of carbon per crop type
  return(bio_inputs)
}
