#' Extract soil profile data from ISRIC SoilGrids using Google Earth Engine.
#'
#' This function extracts soil profile data including Organic Carbon Stock (OCS),
#' Bulk Density (BD), Clay content, Sand content, and Silt content from ISRIC SoilGrids
#' using Google Earth Engine.
#'
#' @param df_latlon A data frame containing latitude and longitude coordinates.
#' @return A list of data frames containing extracted soil profile data.
#'
#' @examples
#' df_latlon <- data.frame(lon = c(-75.5, -76.0), lat = c(42.0, 42.5))
#' soil_data <- get_gee_isric_soil_profile_rothc(df_latlon)
#'
#' @importFrom rgee
#'
#' @export

get_gee_isric_soil_profile_rothc <- function(df_latlon) {
    # Define Earth Engine assets
    ee_assets <- list(
        ocs = "projects/soilgrids-isric/ocs_mean",
        bd = "projects/soilgrids-isric/bdod_mean",
        clay = "projects/soilgrids-isric/clay_mean",
        sand = "projects/soilgrids-isric/sand_mean",
        silt = "projects/soilgrids-isric/silt_mean"
    )

    # Define points of interest
    for (i in 1:nrow(df_latlon)) {
        points_of_interest <-
            c(points_of_interest, list(ee$Geometry$Point(df_latlon$lon[i], df_latlon$lat[i])))
    }
    feature_collection <- ee$FeatureCollection(points_of_interest)

    # Extract values from Earth Engine
    system.time({
        values <- list()
        for (i in seq_along(ee_assets)) {
            layer <- ee$Image(ee_assets[[i]])
            values[[names(ee_assets[i])]] <- ee_extract(
                x = layer,
                y = feature_collection,
                fun = ee$Reducer$mean(),
                scale = 1
            )
        }
    })

    # Process the extracted values
    label <- gsub(".*_(\\d+\\.\\d+)cm.*", "\\1cm", names(values[[2]]))
    result_df <- lapply(1:nrow(values[[2]]), function(j) {
        data.frame(
            label = label[c(1, 5, 3, 4, 6, 2)],
            BD = t(values[["bd"]][c(1, 5, 3, 4, 6, 2)])[, j],
            # Adjust column names as needed
            Carbon = t(values[["ocs"]])[, j],
            ParticleSizeClay = t(values[["clay"]][c(1, 5, 3, 4, 6, 2)])[, j],
            ParticleSizeSand = t(values[["sand"]][c(1, 5, 3, 4, 6, 2)])[, j],
            ParticleSizeSilt = t(values[["silt"]][c(1, 5, 3, 4, 6, 2)])[, j]
        )
    })


    # Set row names for each data frame in the list
    result_df <- lapply(result_df, function(df) {
        rownames(df) <- 1:length(label)
        return(df)
    })

    return(result_df)
}
