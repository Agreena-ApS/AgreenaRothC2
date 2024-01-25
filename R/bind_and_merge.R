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
  read_file <- function(file) {
    ext <- tools::file_ext(file)
    switch(ext,
      csv = read_csv(file),
      xlsx = readxl::read_xlsx(file),
      rds = readRDS(file),
      stop("Unsupported file type")
    )
  }

  data_files <- list()
  for (i in 1:length(files)) {
    data_files[[names(files[i])]] <- bind_rows(lapply(files[[i]], read_file))
  }

  nrows_data <- sapply(data_files, nrow)
  names_data <- names(data_files)
  if (var(nrows_data) > 0) {
    warning(paste("The number of rows in the data sets are different.", paste(names_data, "=", nrows_data, collapse = ", ")))
  }

  # Function to perform left join between two data frames
  left_join_dataframes <- function(df1, df2) {
    # merge(df1, df2, by = "field_id", all = TRUE)
    left_join(df1, df2)
  }

  # Use Reduce() to perform left join on all data frames in the list
  merged_data <- Reduce(left_join_dataframes, data_files)


  # merged_data <- full_join(rothc_data, ipcc_data, by = "field_id")
  # merged_data <- full_join(merged_data, field_data, by = "field_id")
  # missing_field_ids <- list(
  #   rothc_only = setdiff(rothc_data$field_id, union(ipcc_data$field_id, field_data$field_id)),
  #   ipcc_only = setdiff(ipcc_data$field_id, union(rothc_data$field_id, field_data$field_id)),
  #   field_only = setdiff(field_data$field_id, union(rothc_data$field_id, ipcc_data$field_id))
  # )
  # attr(merged_data, "missing_field_ids") <- missing_field_ids
  return(merged_data)
}
