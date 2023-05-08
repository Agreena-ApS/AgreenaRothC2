#' Effects of crop retainment on carbon
#' @title Effect of covered soil on C flows
#' @param x Logical vector with months where the soil is covered. If only a single value is passed the whole period will be treated homogeneously.
#' @description The soil cover factor (c) slows decomposition if growing plants are present. In earlier version of the model this factor is called the 'retainment factor.
#' @return A vector with the rate modifying factor. (0.6 for covered soil and 1 for bare soil)
#' @author Marcos Alves
#' @export
#'

fC_crop_retainment <- function(x) {
  x <- as.logical(x)
  y <- ifelse(x, 0.6, 1)
  if (length(y) == 1) {
    y <- rep(y, 12)
  }
  return(y)
}
