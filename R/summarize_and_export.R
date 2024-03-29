#' Summarize and export calculations
#'
#' This function takes a data frame of emissions reductions and removals calculations
#' and summarizes them by user_id. It also exports the original and summarized data frames
#' to Excel files.
#'
#' @param data A data frame of emissions reductions and removals calculations.
#' @param file_name_original A character string for the name of the Excel file to export the original data frame.
#' @param file_name_summarized A character string for the name of the Excel file to export the summarized data frame.
#'
#' @return A data frame of summarized calculations by user_id.
#'
#' @examples
#' # Create an example data frame
#' data <- data.frame(
#'   user_id = c(1, 1, 2, 2, 3, 3),
#'   field_size_ha = c(10, 15, 12, 18, 20, 25),
#'   total_certs = c(100, 150, 120, 180, 200, 250),
#'   total_certs_ha = c(10, 10, 10, 10, 10, 10),
#'   net_certs_ha = c(6.8, 6.8, 6.8, 6.8, 6.8, 6.8),
#'   buffer = c(20, 30, 24, 36, 40, 50),
#'   fees = c(15, 22.5, 18, 27, 30, 37.5),
#'   net_certs = c(65, 97.5, 78, 117, 130, 162.5),
#'   premium = c(6.5, 9.75, 7.8, 11.7, 13, 16.25),
#'   removals_minus_uncertainty_area = c(50, 75, 60, 90, 100, 125),
#'   reductions_minus_uncertainty_area = c(50, 75, 60, 90, 100, 125)
#' )
#'
#' # Call the function and print the result
#' result <- summarize_and_export(data,
#'   file1 = "all_calcs_example.xlsx",
#'   file2 = "summarized_calcs_example.xlsx"
#' )
#' print(result)
#' @import writexl
#' @import dplyr
#' @export

summarize_and_export <- function(data,
                                 file_name_original,
                                 file_name_summarized) {

  # Summarize the calculations by user_id
  summarized_calc <- data %>%
    group_by(user_id) %>%
    drop_na(VCU_field_eq66) %>%
    summarise(
      total_has = sum(field_size_ha),
      VCU_User = sum(VCU_field_eq66),
      buffer_User = sum(buffer_field),
      fees_User = sum(fees),
      net_certs_user = sum(net_certs),
      premium = sum(premium),
      share_removals_percent_all_users = (sum(Erem_eq38_field) / sum(ERR_eq39_field)) * 100
    )

  # Load the writexl package for exporting to Excel
  library(writexl)

  # Export the original data frame to an Excel file
  writexl::write_xlsx(data, file_name_original)

  # Export the summarized data frame to an Excel file
  writexl::write_xlsx(summarized_calc, file_name_summarized)

  # Return the summarized data frame
  return(summarized_calc)
}
