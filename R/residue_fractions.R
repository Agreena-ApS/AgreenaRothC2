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
  cropname <- trimws(cropname, which = "both") %>% toupper()
  for (i in cropname) {
    fractions <- append(fractions, unlist(res_fractions[i, 1:12]))
  }
  return(fractions)
}


# res_fraction <- yld2bio
# for (i in month.name) {
#   res_fraction <- add_column(res_fraction,i = 0)
# }
# res_fractions <- res_fraction[,1:12]
# colnames(res_fractions) <- month.name
# spt_crops <- c("ALFALFA", " OTHER TUBER CROP", "CLOVER", "GRASS-CLOVER MIX", "PERENNIAL GRASS", "MAIZE", "RICE", "SOYBEAN", "TREE CROP")
# res_fractions[spt_crops,9] <- 1
# res_fractions[!grepl(paste(spt_crops, collapse = "+|"), rownames(res_fraction)), 8] <- 1
# write.csv(res_fractions, "/Users/marcospaulopedrosaalves/Documents/Git/AgreenaRothC/data/res_fractions.csv")
# usethis::use_data(res_fractions, overwrite = T)
