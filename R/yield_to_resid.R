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
  yld2bio <- load_yld2bio()

  coefs <- yld2bio[match(unlist(cropname),yld2bio[, "Coolfarm.name"]), ]
  bio_inputs <- NULL
  for (i in seq_along(cropname)) {
    bio_inputs[i] <-
      (yield[i] * coefs[i, "Dry.matter.fraction.FDM"] * coefs[i, "Slope.a"] * cf1 + coefs[i, "Intercept.b"] * cf2) * 0.15
  }
   # CFT considers a fixed amount of carbon per copr type
  return(bio_inputs)
}
