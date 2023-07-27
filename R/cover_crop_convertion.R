#' @title Cover crop name convertion
#' @description convert a common name to a list of logical values
#' @name cover_crop_convertion
#' @param ccname vector with cropnames
#' @return Vector with monthly values
#' @author Marcos Alves
#' @export

cover_crop_convertion <- function(ccname) {
  # rownames(cc) <- cc[,1]
  cc <- load_cc()
  cover <- NULL
  ccname <- gsub(" ", "", ccname) %>% toupper()
  rownames(cc) <- toupper(rownames(cc))
  for (i in ccname) {
    cover <- append(cover, unlist(cc[i, 1:12]))
  }
  return(cover)
}
