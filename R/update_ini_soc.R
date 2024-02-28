#' Update ini_soc column based on field_id and actual_harvest_year
#'
#' This function checks the number of occurrences of each field_id in the dataset.
#' For field_ids that occur only once, it assigns the value "isric" to the ini_soc column.
#' For field_ids with multiple rows, it assigns the value of the previous year's interval_1 to the ini_soc column.
#'
#' @param data The input dataset
#' @return The updated dataset with the ini_soc column modified
#' @import dplyr
#' @export
update_ini_soc <- function(data) {
    data <- data %>%
        group_by(field_id) %>%
        mutate(
            count = n(),
            oldest_row = min(actual_harvest_year)
        ) %>%
        arrange(actual_harvest_year) %>%
        mutate(
            ini_soc_2023 = ifelse(actual_harvest_year == oldest_row, "isric", lag(final_soc_2022)),
            ini_soc_2023 = ifelse(is.na(ini_soc_2023), "isric", ini_soc_2023)
        ) %>%
        ungroup()
    # select(-count, -oldest_row)
    return(data)
}
