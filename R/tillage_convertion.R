#' @title Tillage convertion factor
#' @description Select the convertion factor depending on the tillage regime
#' @name tillage_convertion
#' @param tillage vector with tillage regimes
#' @return Vector with monthly values
#' @author Marcos Alves
#' @export

tillage_convertion <- function(tillage) {
  rownames(tillage_convert) <- toupper(rownames(tillage_convert))
  till_cr <- NULL
  tillage <- gsub(" ", "", tillage) %>% toupper()
  for (i in tillage) {
    till_cr <- append(till_cr,unlist(tillage_convert[i,]))
  }
  return(till_cr)
}
