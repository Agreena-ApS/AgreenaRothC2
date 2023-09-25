#' Write out data to CSV files with CO2 SOC removals information.
#'
#' This function takes a data frame 'out', which contains information about CO2
#' SOC (Soil Organic Carbon) removals for various fields over multiple years.
#' It performs additional data transformations and writes the results to multiple CSV files:
#'
#'    1. A CSV file containing CO2 SOC removals per year per field during the project duration.
#'    2. A CSV file containing the total CO2 SOC removals per year across all fields during the project duration.
#'    3. A CSV file containing CO2 SOC removals per field for the harvest year (and a copy in .rds)
#'
#' @param out A data frame containing the input data, including columns 'field_id', 'year', and 'co2_removals'.
#'             'field_id' uniquely identifies each field, 'year' represents the year of observation,
#'             and 'co2_removals' contains the recorded CO2 SOC removals for that year and field.
#'
#' @param field_data A data frame containing additional information about the fields,
#'                   including 'field_id' and 'field_size_ha', which indicates the size of each field in hectares.
#'                   This information is used to calculate the total CO2 SOC removals per field.
#'
#' @return This function doesn't return anything directly, but it writes the processed data to CSV files.
#'
#' @import tidyverse
#' @import dplyr
#' @import tidyr
#' @export
#'
#' @examples
#' # Example usage:
#' # write_out(out_data, field_data)
write_out <- function(out, field_data, sim_period) {
  
  start_year <- as.numeric(format(as.Date(sim_period[1], format = "%Y-%m-%d"), "%Y"))
  end_year <- as.numeric(format(as.Date(sim_period[2], format = "%Y-%m-%d"), "%Y"))
  
  # Create a vector of years from start to end
  sim_period <- start_year:end_year
  
  saveRDS(out, file.path(
    getOption("output_version_folder"),
    paste0("output_raw_", getOption("version"), ".rds")
  ))
  
  out_current_harvest_year <- as.data.frame(out) %>% dplyr::filter(year == 1)
  
  write.csv(
    out_current_harvest_year,
    file.path(
      getOption("output_version_folder"),
      paste0("CO2_removals_per_field_harvest_year_", getOption("version"), ".csv")
    )
  )
  
  saveRDS(
    out_current_harvest_year,
    file.path(
      getOption("output_version_folder"),
      paste0("CO2_removals_per_field_harvest_year_", getOption("version"), ".rds")
    )
  )
  
  interval_cols <- grep("^Interval", names(out), value = TRUE)
  if(!any(grepl("size", names(out)))) {out <- dplyr::inner_join(as.data.frame(out), field_data[, c("field_id", "field_size_ha")], by = "field_id")}
  out[, interval_cols] <- out$field_size_ha * out[, interval_cols]

  process_data <- function(out) {
    wide_out <- list()
    wide_out_field <- list()
    for (colname in interval_cols) {
      wide_out[[colname]] <- out %>%
        select(field_id, year, colname) %>%
        tidyr::pivot_wider(names_from = year, values_from = colname) %>%
        select(matches("[0-9]")) %>%
        colSums()
      names(wide_out[[colname]]) <- sim_period
      wide_out_field[[colname]] <- out %>%
        select(field_id, year, colname) %>%
        tidyr::pivot_wider(names_from = year, values_from = colname)
      names(wide_out_field[[colname]]) <- c("field_id", 2022:2041)
    }
    return(list(
      "wide_out" = as.data.frame(wide_out),
      "wide_out_field" = as.data.frame(wide_out_field)
    ))
  }

  out_processed <- process_data(out)

  write.csv(out_processed$wide_out, file.path(
    getOption("output_version_folder"),
    paste0("Avg_program_CO2_removals_per_year_", getOption("version"), ".csv")
  ))

  write.csv(out_processed$wide_out_field, file.path(
    getOption("output_version_folder"),
    paste0("Program_CO2_SOC_removals_per_year_per_field_", getOption("version"), ".csv")
  ))
}