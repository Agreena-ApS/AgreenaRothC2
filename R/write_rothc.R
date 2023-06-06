#' @export
#'

write_rothc <- function(out) {
  write.csv(out, file.path(getOption("output_version_folder"), paste0("Summarized_outputs_", getOption("version"), ".csv")))
  saveRDS(out, file.path(getOption("output_version_folder"), paste0("Summarized_outputs_", getOption("version"), ".rds")))
}
