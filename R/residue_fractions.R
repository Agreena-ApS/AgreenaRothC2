#' @title Fraction of residues
#' @description Select the amount of the residues being left on the field in each month
#' @name residue_fractions
#' @param cropname vector with cropnames
#' @return Vector with monthly values
#' @author Marcos Alves
#' @export

residue_fractions <- function(cropname) {
  rownames(res_fractions) <- toupper(rownames(res_fractions))
  fractions <- NULL
  cropname <- trimws(cropname, which = "both") %>%
    toupper()

  for (i in cropname) {
    fractions <- append(fractions, unlist(res_fractions[i, 1:12]))
  }

  return(fractions)
}
