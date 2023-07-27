#' @title Tillage convertion factor
#' @description Select the convertion factor depending on the tillage regime
#' @name tillage_conversion
#' @param tillage vector with tillage regimes
#' @return Vector with monthly values
#' @author Marcos Alves
#' @export

# tillage_conversion <- function(tillage) {
#   rownames(tillage_convert) <- toupper(rownames(tillage_convert))
#   till_cr <- NULL
#   tillage <- gsub(" ", "", tillage) %>% toupper()
#   for (i in tillage) {
#     till_cr <- append(till_cr,unlist(tillage_convert[i,]))
#   }
#   return(till_cr)
# }



tillage_conversion <- function(tillage) {
  tillage_convert <- load_tillage_convert()
  tillage <- gsub(" ", "", tillage) %>% toupper()
  till_cr <- match(tillage, tillage_convert$Tillage_Type) %>%
    tillage_convert[., ] %>%
    pull() %>%
    unlist()
  return(till_cr)
}
