#' Source: https://www.isric.org/ \cr
#' Details: https://www.isric.org/explore/soilgrids/faq-soilgrids \cr
#'
#' @title Download a soil profile from the ISRIC soil database
#' @description Retrieves soil data from the ISRIC global database and converts it to a data frame that can be used to run RothC
#' @name get_isric_soil_profile_rothc
#' @param lonlat Longitude and latitude vector (e.g. c(-93, 42)).
#' @param statistic default is the mean
#' @param find.location.name default is TRUE. Use either maps package or photon API to find Country/State.
#' If you are running this function many times it might be better to set this to FALSE.
#' @return data frame with soil characteristics and \code{meta} Attributes.
#' @details Variable which are directly retrieved and a simple unit conversion is performed: \cr
#' * Bulk density - bdod \cr
#' * Carbon - ocs \cr
#' * Clay - clay \cr
#' * Sand - sand \cr
#' * Silt - silt \cr
#' \code{attr(x, "meta")} loads meta information about the location from where the soil profile was created.
#' @import jsonlite maps
#' @export
#' @author Marcos Alves (Modified from: Fernando E. Miguez)
#' @examples
#' \dontrun{
#' ## Get soil profile properties for a single point
#' sp1 <- get_isric_soil_profile_rothc2(lonlat = c(-93, 42))
#' sp1
#' attr(sp1, "meta")
#' }
#'
get_isric_soil_profile_rothc2 <- function(lonlat,
                                          statistic = c("mean", "Q0.5"),
                                          find.location.name = TRUE) {
    statistic <- match.arg(statistic)

    #### Create extent step ####
    lon <- as.numeric(lonlat[1])
    lat <- as.numeric(lonlat[2])
    lon_initial <- as.numeric(lonlat[1])
    lat_initial <- as.numeric(lonlat[2])

    if (lon < -180 || lon > 180) stop("longitude should be between -180 and 180")
    if (lat < -90 || lat > 90) stop("latitude should be between -90 and 90")

    retrieve_soil <- function(lon, lat, statistic) {
        ##  rest0 <- "https://rest.soilgrids.org/soilgrids/v2.0/properties/query?lon="
        base_query_url <- "https://farm-data-api-7vy7lq4sca-ew.a.run.app/soilgrids/property/query?"
        base_with_lonlat <- paste0(base_query_url, "lon=", lon, "&lat=", lat)
        properties_in_order <- c("bdod", "ocs", "clay", "sand", "silt")

        queries_with_properties <- paste(
            base_with_lonlat,
            sprintf("property=%s", properties_in_order),
            sep = "&"
        )

        res_all_queries <- lapply(queries_with_properties, jsonlite::fromJSON)
        names(res_all_queries) <- properties_in_order
        return(res_all_queries)
    }

    set.seed(123)
    n <- 10
    c <- 0.001
    rest.data <- retrieve_soil(lon, lat, statistic)
    ocs <- rest.data$ocs$properties$layers$depths[[1]][, 3]

    #### Process query
    sp.nms <- sapply(rest.data, function(x) {
        x$properties$layers$name
    })

    if (!all(sp.nms %in% c("bdod", "ocs", "clay", "sand", "silt"))) {
        cat("Found these properties", sp.nms, "\n")
        cat("Expected these properties", c("bdod", "ocs", "clay", "sand", "silt"), "\n")
        stop("soil properties names do not match")
    }



    # if(any(is.na(ocs))) stop("No soil data available for this location. Did you specify the coordinates correctly?")
    # Algorithm to deal with missing/invalid lat and long values
    if (any(isTRUE(is.na(ocs) | is.null(ocs)))) {
        while (any(is.na(ocs))) {
            lon <- runif(n, min = lon - c, max = lon + c)
            lat <- runif(n, min = lat - c, max = lat + c)
            lonlat_grid <- expand.grid(lon, lat)
            lonlat <- lonlat_grid[sample(1:n^2, n), ]
            for (i in 1:n) {
                lon <- as.numeric(lonlat[i, 1])
                lat <- as.numeric(lonlat[i, 2])
                rest.data <- retrieve_soil(lon, lat, statistic)
                ocs <- rest.data$ocs$properties$layers$depths[[1]][, 3]
                if (!any(is.na(ocs))) {
                    warning(paste("Coordinates where altered from (lon, lat):", lon_initial, lat_initial, "->", lon, lat))
                    break
                }
            }
            c <- sqrt(c)
        }
    }

    bdod <- rest.data$bdod$properties$layers$depths[[1]][1:6, 3]
    clay <- rest.data$clay$properties$layers$depths[[1]][1:6, 3]
    sand <- rest.data$sand$properties$layers$depths[[1]][1:6, 3]
    silt <- rest.data$silt$properties$layers$depths[[1]][1:6, 3]


    ### For some of the conversions see: https://www.isric.org/explore/soilgrids/faq-soilgrids
    soil_profile <- NULL
    soil_profile$label <- rest.data[[1]]$properties$layers$depths[[1]]["label"][1:6, ]
    soil_profile$BD <- bdod[[1]] * 1e-2
    soil_profile$Carbon <- ocs[[1]]
    soil_profile$ParticleSizeClay <- clay[[1]] * 1e-1
    soil_profile$ParticleSizeSand <- sand[[1]] * 1e-1
    soil_profile$ParticleSizeSilt <- silt[[1]] * 1e-1
    soil_profile <- as.data.frame(soil_profile)

    #### Passing parameters from soilwat
    ## The soil texture class will be based on the first layer only
    txt_clss <- texture_class(soil_profile$ParticleSizeClay[1] * 1e-2, soil_profile$ParticleSizeSilt[1] * 1e-2)

    if (find.location.name) {
        if (requireNamespace("maps", quietly = TRUE)) {
            country <- maps::map.where(x = lon, y = lat)
            if (country == "USA") {
                state <- toupper(maps::map.where(database = "county", x = lon, y = lat))
            } else {
                url <- paste0("https://photon.komoot.io/reverse?lon=", lon, "&lat=", lat)
                fgeo <- jsonlite::fromJSON(url)
                state <- fgeo$feature$properties$state
            }
        } else {
            url <- paste0("https://photon.komoot.io/reverse?lon=", lon, "&lat=", lat)
            fgeo <- jsonlite::fromJSON(url)
            state <- fgeo$feature$properties$state
            country <- fgeo$features$properties$country
        }
    } else {
        state <- NULL
        country <- NULL
    }


    #### Attributes ####
    alist <- list()
    alist$SoilType <- txt_clss
    alist$State <- state
    alist$Country <- country
    alist$Longitude <- lon
    alist$Latitude <- lat
    alist$ini_Longitude <- lon_initial
    alist$ini_Latitude <- lat_initial
    alist$DataSource <- paste("Original source is www.isric.org. See: https://www.isric.org/explore/soilgrids/faq-soilgrids ", Sys.time())
    alist$Comments <- paste(
        "resolution = 250m",
        "- taxonomic classification name =", txt_clss
    )
    attr(soil_profile, "meta") <- alist

    return(soil_profile)
}

## Potentially useful function provided by Eric Zurcher
## written by Andrew Moore
## I think the values should be in the 0-1 range
# ==========================================================================
# Texture class mapping function
# ==========================================================================

# Re-express the PSD in terms of the International system, using an equation from Minasny et al. (2001)

intl_clay_propn <- function(usda_clay, usda_silt) {
    return(usda_clay)
}

intl_silt_propn <- function(usda_clay, usda_silt) {
    return(max(0.0, -0.0041 - 0.127 * usda_clay + 0.553 * usda_silt + 0.17 * usda_clay^2 - 0.19 * usda_silt^2 + 0.59 * usda_clay * usda_silt))
}

intl_sand_propn <- function(usda_clay, usda_silt) {
    return(1.0 - intl_clay_propn(usda_clay, usda_silt) - intl_silt_propn(usda_clay, usda_silt))
}

# Texture triangle as equations

texture_class <- function(usda_clay, usda_silt) {
    if (usda_clay < 0 || usda_clay > 1) stop("usda_clay should be between 0 and 1")
    if (usda_silt < 0 || usda_silt > 1) stop("usda_silt should be between 0 and 1")

    intl_clay <- intl_clay_propn(usda_clay, usda_silt)
    intl_silt <- intl_silt_propn(usda_clay, usda_silt)
    intl_sand <- 1.0 - intl_clay - intl_silt

    if ((intl_sand < 0.75 - intl_clay) && (intl_clay >= 0.40)) {
        class <- "silty clay"
    } else if ((intl_sand < 0.75 - intl_clay) && (intl_clay >= 0.26)) {
        class <- "silty clay loam"
    } else if (intl_sand < 0.75 - intl_clay) {
        class <- "silty loam"
    } else if ((intl_clay >= 0.40 + (0.305 - 0.40) / (0.635 - 0.35) * (intl_sand - 0.35)) && (intl_clay < 0.50 + (0.305 - 0.50) / (0.635 - 0.50) * (intl_sand - 0.50))) {
        class <- "clay"
    } else if (intl_clay >= 0.26 + (0.305 - 0.26) / (0.635 - 0.74) * (intl_sand - 0.74)) {
        class <- "sandy clay"
    } else if ((intl_clay >= 0.26 + (0.17 - 0.26) / (0.83 - 0.49) * (intl_sand - 0.49)) && (intl_clay < 0.10 + (0.305 - 0.10) / (0.635 - 0.775) * (intl_sand - 0.775))) {
        class <- "clay loam"
    } else if (intl_clay >= 0.26 + (0.17 - 0.26) / (0.83 - 0.49) * (intl_sand - 0.49)) {
        class <- "sandy clay loam"
    } else if ((intl_clay >= 0.10 + (0.12 - 0.10) / (0.63 - 0.775) * (intl_sand - 0.775)) && (intl_clay < 0.10 + (0.305 - 0.10) / (0.635 - 0.775) * (intl_sand - 0.775))) {
        class <- "loam"
    } else if (intl_clay >= 0.10 + (0.12 - 0.10) / (0.63 - 0.775) * (intl_sand - 0.775)) {
        class <- "sandy loam"
    } else if (intl_clay < 0.00 + (0.08 - 0.00) / (0.88 - 0.93) * (intl_sand - 0.93)) {
        class <- "loamy sand"
    } else {
        class <- "sand"
    }

    return(class)
}
