#' @export
#'

write_field_data <- function(field_data){
  saveRDS(field_data, file.path(getOption("output_version_folder"), paste0("field_data_",getOption("version"),".rds")))
}
