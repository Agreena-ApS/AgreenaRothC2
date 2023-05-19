#' @export

config <- function(cfg){

  farm_input_folder <- file.path(getOption("root")) %>% file.path("farm_data")
  data_input_folder <- file.path(getOption("root")) %>% file.path("assumptions_data")
  getOption("root") %>% setwd()

  if (!dir.exists(data_input_folder)) {
    dir.create(data_input_folder)
    warning(paste0("Data assumptions folder created: ", data_input_folder))
    warning(paste0("Add assumption files : org_fert_c_rate.csv and crop_names.csv"))
  }

  output_folder <- file.path(getOption("root")) %>% file.path("output_rothc")
  output_version_folder <- file.path(output_folder, paste0(cfg[["version"]], "_", Sys.time() %>% format("%d_%m_%Y_%H%M")))
  if (!dir.exists(output_version_folder)) {
    dir.create(output_folder,  showWarnings = FALSE)
    dir.create(output_version_folder)
    warning(paste0("Output folder created: ",output_version_folder))
  }

  if (!dir.exists(farm_input_folder)) {
    dir.create(farm_input_folder)
  }
  input_version_folder <- file.path(farm_input_folder, cfg[["version"]])
  if (!dir.exists(input_version_folder)) {
    dir.create(input_version_folder)
    stop("Version input folder created. Add running files to the folder")
  } else {
    files <- list.files(input_version_folder)
    if (length(files)<2) {
      stop("Less than 2 input files")
    }
    field_data <- files[grep("Base",files)]
    fert_data <- files[grep("Fert",files)]

    split_string <- strsplit(field_data, ".", fixed = TRUE)[[1]]
    field_data_new <- paste0(split_string[1],"_",cfg[["version"]], ".", split_string[2])

    split_string <- strsplit(fert_data, ".", fixed = TRUE)[[1]]
    fert_data_new <- paste0(split_string[1],"_",cfg[["version"]], ".", split_string[2])
    files_new <- c(field_data_new, fert_data_new)
  }

  # output_folder <- file.path(getOption("root")) %>% dirname() %>% file.path("output_rothc")
  #
  # if (!dir.exists(output_folder)) {
  #   dir.create(output_folder)
  # }
  # output_version_folder <- file.path(output_folder, cfg[["version"]])
  # if (!dir.exists(output_version_folder)) {
  #   dir.create(output_version_folder)
  #   warning(paste0("Output folder created: ", cfg[["version"]]))
  # }

  for (i in 1:length(files_new)) {
    file.copy(file.path(input_version_folder,files[i]), file.path(output_version_folder, files_new[i]),overwrite = T)
  }


  assumptions_data_output_version_folder <- file.path(output_version_folder, paste0("assumptions_data_", cfg[["version"]]))
  dir.create(assumptions_data_output_version_folder)
  file.copy(file.path(data_input_folder,cfg[["org_fert_c_rate"]]), file.path(assumptions_data_output_version_folder, cfg[["org_fert_c_rate"]]),overwrite = T)
  file.copy(file.path(data_input_folder,cfg[["crop_names"]]), file.path(assumptions_data_output_version_folder, cfg[["crop_names"]]), overwrite = T)

 options(field_data = file.path(output_version_folder, field_data_new))
 options(fert_data = file.path(output_version_folder, fert_data_new))
 options(output_version_folder = output_version_folder)
 options(version = cfg[["version"]])
 options(org_fert_c_rate = cfg[["org_fert_c_rate"]])
 options(crop_names = cfg[["crop_names"]])
 options(data_source = data_input_folder)
}
