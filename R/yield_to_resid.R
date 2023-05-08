#' @title Yield to residues
#' @description Calculate crop specific biomass residues
#' baseline and scenario runs.
#' @name yield_to_resid
#' @param yield vector with crop yields
#' @param cropname vector with cropnames
#' @param cf1 Coefficient factor for slope yield inputs
#' @param cf2 Coefficient factor Intercept yield inputs
#' @return vector with crop residues in t C/ha
#' @author Marcos Alves
#' @export

yield_to_resid <- function(yield, cropname, cf1 = 1, cf2 = 1) {

  cropname <- trimws(cropname, which = "both") %>% toupper()
  cropname <- agreena2cft(cropname)
  coefs <- yld2bio[unlist(cropname), ]
  bio_inputs <- NULL
  for (i in seq_along(cropname)) {
    bio_inputs[i] <-
      (yield[i] * coefs[cropname[i], "Dry.matter.fraction.FDM"] * coefs[cropname[i], "Slope.a"] * cf1 + coefs[cropname[i], "Intercept.b"] * cf2) * 0.15
  }
   # CFT considers a fixed amount of carbon per copr type
  return(bio_inputs)
}
