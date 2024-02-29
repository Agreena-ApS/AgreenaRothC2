#' Source:https://esdac.jrc.ec.europa.eu/projects/lucas/ \cr
#'
#' @title Download a soil profile from the Lucas soil database
#' @description Retrieves soil data from the lucas database and converts it to a data frame that can be used to run RothC
#' @name get_lucas_soil_profile_rothc
#' @param lonlat Longitude and latitude vector (e.g. c(-93, 42)).
#' @return data frame with soil characteristics and \code{meta} Attributes.
#' @details Variable which are directly retrieved: \cr
#' * Bulk density - BD (g/cm3)\cr
#' * Carbon - SOC (tC/ha) \cr
#' * Clay - ParticleSizeClay (%)\cr
#' @author Marcos Alves
#' @examples
#' \dontrun{
#' ## Get soil profile properties for a single point
#' sp1 <- get_lucas_soil_profile_rothc(lonlat = c(-93, 42))
#' sp1
#' attr(sp1, "meta")
#' }
#' @export
#'

get_lucas_soil_profile_rothc <- function(lonlat) {
  soil <- data.frame(label = c("0-10cm", "10-20cm", "20-30cm"), Carbon = 1:3, ParticleSizeClay = 1:3)
  distancia <- function(x1, x2) {
    return(dist(rbind(x1, x2)))
  }
  y <- apply(lucas[, c("TH_LONG", "TH_LAT")], 1, distancia, lonlat)
  lucas_soil <- lucas[match(min(y), y), c("Depth", "SOC", "CLAY", "BD_O")]
  carbon <- rep(sum(as.numeric(lucas_soil$SOC) / (2 / 3)), 3) # assuming carbon density is constant between 0-20 and 20-30 cm and extrapolating (regra de 3)
  soil$ParticleSizeClay <- rep(lucas_soil$CLAY, 3)
  soil$Carbon <- carbon
  soil$BD <- lucas_soil$BD_O
  attr(soil, "meta")$Longitude <- lucas[match(min(y), y), c("TH_LONG")]
  attr(soil, "meta")$Latitude <- lucas[match(min(y), y), c("TH_LAT")]
  return(soil)
}
