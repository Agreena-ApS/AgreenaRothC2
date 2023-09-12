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

write_out <- function(out, field_data) {
  
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
  
  # Inner join with field_data to add 'field_size_ha' column
  out <- dplyr::inner_join(as.data.frame(out), field_data[,c("field_id", "field_size_ha")], by = "field_id")
  out$co2_removals_total <- out$field_size_ha * out$co2_removals_all_years_last_month
  
  out <- as_tibble(out)
  
  # Pivot to get wide format
  wide_out <- out %>% select(field_id,year,co2_removals_all_years_last_month) %>% tidyr::pivot_wider(names_from = year, values_from = co2_removals_all_years_last_month)
  wide_out_field_avg <- out %>% select(field_id,year,co2_removals_total) %>%  tidyr::pivot_wider(names_from = year, values_from = co2_removals_total) %>% select(matches("[0-9]")) %>% colSums()
  names(wide_out) <- c("field_id", 2022:2041)
  names(wide_out_field_avg) <- c(2022:2041)
  wide_out_field_avg <- as.data.frame(wide_out_field_avg)
  colnames(wide_out_field_avg) <- "SOC removals in tC02eq"
  
  # Write to CSV files
  write.csv(wide_out_field_avg, file.path(
    getOption("output_version_folder"),
    paste0("Avg_program_CO2_removals_per_year_", getOption("version"), ".csv")
  ))
  
  write.csv(as.data.frame(wide_out), file.path(
    getOption("output_version_folder"),
    paste0("Program_CO2_SOC_removals_per_year_per_field_", getOption("version"), ".csv")
  ))
}