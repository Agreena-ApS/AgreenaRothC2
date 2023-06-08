#' Bind and merge RothC and ipcc data sets
#'
#' This function takes a list of file names for RothC and ipcc data sets,
#' row binds them separately, and then merges them by field_id.
#'
#' @param files A list of file names for RothC and ipcc data sets.
#' The list should have two named elements: "RothC" and "ipcc",
#' each containing a vector of file names as sublists.
#'
#' @return A data frame with the merged RothC and ipcc data.
#'
#' @examples
#' files <- list(
#'   "RothC" = list("file1.csv", "file2.csv", "file3.csv"),
#'   "ipcc" = list("file1.csv", "file2.csv", "file3.csv")
#' )
#' result <- bind_and_merge(files)
#' print(result)
#'
#' @import dplyr
#' @export

bind_and_merge <- function(files) {
  library(dplyr)
  read_file <- function(file) {
    ext <- tools::file_ext(file)
    switch(ext,
      csv = read.csv(file),
      xlsx = readxl::read_xlsx(file),
      rds = readRDS(file),
      stop("Unsupported file type")
    )
  }
  rothc_data <- bind_rows(lapply(files$RothC, read_file))
  ipcc_data <- bind_rows(lapply(files$ipcc, read_file))
  field_data <- bind_rows(lapply(files$field_data, read_file))
  if (nrow(rothc_data) != nrow(ipcc_data) | nrow(rothc_data) != nrow(field_data)) {
    warning("The number of rows in the data sets are different.")
  }
  merged_data <- full_join(rothc_data, ipcc_data, by = "field_id")
  merged_data <- full_join(merged_data, field_data, by = "field_id")
  missing_field_ids <- list(
    rothc_only = setdiff(rothc_data$field_id, union(ipcc_data$field_id, field_data$field_id)),
    ipcc_only = setdiff(ipcc_data$field_id, union(rothc_data$field_id, field_data$field_id)),
    field_only = setdiff(field_data$field_id, union(rothc_data$field_id, ipcc_data$field_id))
  )
  attr(merged_data, "missing_field_ids") <- missing_field_ids
  return(merged_data)
}
