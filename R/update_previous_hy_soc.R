#' Replace values in a data frame with values from a CSV file
#'
#' This function replaces the values in the original data frame with the values from a CSV file
#' for the rows where 'field_id' and 'actual_harvest_year' are the same and the column names are the same in both data frames.
#'
#' @param csv_file The path to the CSV file.
#' @param df The original data frame.
#'
#' @return A data frame with replaced values.
#'
#' @examples
#' \dontrun{
#' replace_values("path/to/your/csv_file.csv", your_data_frame)
#' }
#' @export

update_previous_hy_soc <- function(csv_file, df) {
    df_csv <- read.csv(csv_file) %>% as_tibble()
    # Temporary workaround to run HY 2023
    df_csv <- df_csv %>% rename(ini_soc_2022 = ini_SOC)
    current_year <- 2022
    df_csv$actual_harvest_year <- current_year
    col_name <- paste("final_soc", current_year, sep = "_")
    df_csv[[col_name]] <- df_csv$ini_soc_2022 + df_csv$Interval_1

    # Identify the column names of interest
    cols <- c("field_id", "actual_harvest_year", "ini_soc_2022", col_name)

    # For each common column, update the values in the original data frame
    # where the 'field_id' and 'actual_harvest_year' are the same

    df <- df %>%
        left_join(df_csv[, cols], by = c("field_id", "actual_harvest_year"))

    return(df)
}
