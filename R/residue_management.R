#' @title Fraction of residues
#' @description Select the amount of the residues being left on the field in each month
#' @name residue_management
#' @param res_mgmt vector management options
#' @return Vector with monthly values
#' @author Marcos Alves
#' @export

residue_management <- function(res_mgmt) {
  rownames(resid_mgmt) <- toupper(rownames(resid_mgmt))
  res <- NULL
  res_mgmt <- gsub(" ", "", res_mgmt) %>% toupper()
  for (i in res_mgmt) {
    res <- append(res,unlist(resid_mgmt[i,1:12]))
  }
  return(res)
}

