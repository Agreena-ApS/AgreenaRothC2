#' This function requires the \CRANpkg{nasapower} package version 4.0.0.
#' @md
#' @title Download NASA-POWER weather data
#' @description Uses \code{\link[nasapower]{get_power}} from the \CRANpkg{nasapower} package to download data and calculate yearly means and standard deviation of temperature, precipitation and evapotranspiration.
#' @name get_wth_power_nasa
#' @param lonlat Longitude and latitude vector
#' @param dates Data range defining the period that should be used to create the ["Climatolocal Normals"](https://en.wikipedia.org/wiki/Climatological_normal) for the specified locations.
#' @return returns a data frame with average (AV) and standard deviation (SD) of surface temperature (TS), bias corrected precipitation (PRECTOTCORR) and Evapotranspiration Energy Flux (EVPTRNS).
#' @details A summary description of nasa power variables can be found here: https://gist.github.com/abelcallejo/d68e70f43ffa1c8c9f6b5e93010704b8
#' TODO: Check the unit measures of the different outputs
#' @author Marcos Alves
#' @import nasapower
#' @import SoilR
#' @import dplyr
#' @export
#' @examples
#' \dontrun{
#' require(nasapower)
#' pwr <- get_wth_power_nasa(lonlat = c(-93, 42), dates = c("2012-01-01", "2012-12-31"))
#' }
#'
get_wth_power_nasa <- function(lonlat, dates) {
  if (!requireNamespace("nasapower", quietly = TRUE)) {
    warning("The nasapower package is required for this function")
    return(NULL)
  }
  if (packageVersion("nasapower") <= "3.0.1") {
    stop("Please upgrade the 'nasapower' package to the latest version",
      call. = FALSE
    )
  }

  if (as.Date(dates[2]) - as.Date(dates[1]) < 700) {
    warning("Dates internval smaller than 24 months. Standard deviation estimations will not be calculated for all months of the year.",
      call. = FALSE
    )
  }

  pwr <-
    nasapower::get_power(
      community = "AG",
      pars = c("TS", "PRECTOTCORR", "EVPTRNS"),
      dates = dates,
      lonlat = lonlat,
      temporal_api = "daily"
    )
  pwr <-
    pwr %>%
    group_by(MM, YEAR) %>%
    summarise(
      TS_AVT = mean(TS),
      TS_SDT = sd(TS),
      PRECTOTCORR_SUM = sum(PRECTOTCORR),
      EVPTRNS_SUM = sum(EVPTRNS),
    ) %>%
    group_by(MM) %>%
    summarise(
      TS_AV = mean(TS_AVT),
      TS_SD = sd(TS_AVT),
      PRECTOTCORR_AV = mean(PRECTOTCORR_SUM),
      PRECTOTCORR_SD = sd(PRECTOTCORR_SUM),
      EVPTRNS_AV = mean(EVPTRNS_SUM),
      EVPTRNS_SD = sd(EVPTRNS_SUM)
    )

  alist <- list()

  alist$units <- c("()", "()", "(oC)", "(oC)", "(mm)", "(mm)", "(?)", "(?)")
  alist$comments <- paste(
    "!data from nasapower R package. retrieved: ",
    Sys.time()
  )
  alist$longitude <- paste(lonlat[1])
  alist$latitude <- paste(lonlat[2])
  alist$dates <- dates

  attr(pwr, "meta") <- alist
  return(pwr)
}
