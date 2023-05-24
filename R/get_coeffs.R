#' @import jsonlite
#' @import httr
#' @export
get_coeffs <- function(lon, lat) {
  retrive_api <- function(lon,lat){
  coeficient_request_body <- paste0('{
  "collections": [
    "climate_zone_coefficients"
  ],
  "intersects": {
        "coordinates": [',lon,",",lat, '],
        "type": "Point"
      }
 }')
  res <- POST("https://stac-api-ab6bkjb4oq-nw.a.run.app/search", body = coeficient_request_body, encode = "json")
  data = fromJSON(rawToChar(res$content))
  coeffs <- data$features$properties$coefficients
  return(coeffs)
  }
  coeffs <- retrive_api(lon, lat)

  # n <- 10
  # c <- 0.001
  # if (is.null(coeffs)) {
  #   while (is.null(coeffs)) {
  #     print(c(lat, lon))
  #     lon <- runif(n, min = lon - c, max = lon + c)
  #     lat <- runif(n, min = lat - c, max = lat + c)
  #     lonlat_grid <- expand.grid(lon, lat)
  #     lonlat <- lonlat_grid[sample(1:n^2, n), ]
  #     for (i in 1:n) {
  #       lon <- as.numeric(lonlat[i, 1])
  #       lat <- as.numeric(lonlat[i, 2])
  #       rest.data <- retrive_api(lon, lat)
  #       if (!is.null(coeffs)) {
  #         warning(paste("Coordinates where altered from (lon, lat):", lon_initial, lat_initial, "->", lon, lat))
  #         break
  #       }
  #     }
  #     c <- sqrt(c + c)
  #   }
  # }
  return(coeffs)
}





