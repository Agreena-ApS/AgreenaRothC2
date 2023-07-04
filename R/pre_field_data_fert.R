#' @importFrom dplyr mutate_if mutate if_else filter select contains left_join
#' @importFrom tidyr pivot_longer
#' @importFrom readr read_lines read_delim read_xlsx drop_na
#' @export

pre_field_data_fert <- function(actuals_year) {
  get_file_delimiter <- function(file_path) {
    file_extension <- tools::file_ext(file_path)

    if (file_extension == "csv") {
      first_line <- readr::read_lines(file_path, n_max = 1)
      if (grepl(",", first_line)) {
        delimiter <- ","
      } else if (grepl(";", first_line)) {
        delimiter <- ";"
      } else {
        delimiter <- ","
        message("No delimiter detected. Assuming comma (',') as the default delimiter.")
      }
    } else if (file_extension == "xlsx") {
      delimiter <- ","
      message("Opening an Excel file. Assuming comma (',') as the default delimiter.")
    } else {
      stop("Unsupported file format. Only .csv and .xlsx files are supported.")
    }

    return(delimiter)
  }

  delimiter1 <- get_file_delimiter(getOption("field_data"))
  delimiter2 <- get_file_delimiter(getOption("fert_data"))

  file_extension1 <- tools::file_ext(getOption("field_data"))
  file_extension2 <- tools::file_ext(getOption("fert_data"))

  if (file_extension1 == "csv") {
    field_data <- readr::read_delim(getOption("field_data"), delim = delimiter1)
  } else if (file_extension1 == "xlsx") {
    field_data <- readxl::read_xlsx(getOption("field_data"))
  } else {
    stop("Unsupported file format. Only .csv and .xlsx files are supported.")
  }

  if (file_extension2 == "csv") {
    fert_data <- readr::read_delim(getOption("fert_data"), delim = delimiter2)
  } else if (file_extension2 == "xlsx") {
    fert_data <- readxl::read_xlsx(getOption("fert_data"))
  } else {
    stop("Unsupported file format. Only .csv and .xlsx files are supported.")
  }

  field_data <- field_data %>%
    dplyr::mutate_if(
      is.character,
      function(x) ifelse(is.na(as.numeric(x)), x, as.numeric(x))
    )

  data_source <- getOption("data_source")

  org_c <- read.csv(file.path(data_source, getOption("org_fert_c_rate")))
  rownames(org_c) <- org_c[, 2]
  fert_data <- fert_data %>%
    dplyr::mutate(
      actual_harvest_year = dplyr::if_else(
        is.na(actual_harvest_year),
        9999,
        as.numeric(actual_harvest_year)
      )
    ) %>%
    dplyr::filter(
      actual_harvest_year == actuals_year,
      actual_fertilisers_mixed != "Only synthetic"
    ) %>%
    dplyr::select(
      "field_id",
      dplyr::contains("actual_fertiliser_id_"),
      dplyr::contains("fertiliser_actual_application_rate"),
      dplyr::contains("fertiliser_actual_fertiliser_name")
    )
  fert_names <- tidyr::pivot_longer(fert_data,
    cols = dplyr::contains("fertiliser_actual_fertiliser_name_0"),
    names_to = letters[1:5],
    names_sep = "_",
    values_to = "name"
  ) %>%
    dplyr::select("field_id", "e", "name")
  fert_rates <- tidyr::pivot_longer(fert_data,
    cols = dplyr::contains("fertiliser_actual_application_rate_0"),
    names_to = letters[1:5],
    names_sep = "_",
    values_to = "Application_rate"
  ) %>%
    dplyr::select("field_id", "e", "Application_rate")
  fert_id <- tidyr::pivot_longer(fert_data,
    cols = dplyr::contains("fertiliser_actual_fertiliser_id"),
    names_to = letters[1:5],
    names_sep = "_",
    values_to = "fert_id"
  ) %>%
    dplyr::select("field_id", "e", "fert_id")
  fert_long <- dplyr::left_join(fert_id, fert_rates, by = c("field_id", "e")) %>%
    dplyr::left_join(fert_names, by = c("field_id", "e")) %>%
    tidyr::drop_na()

  fert_long$c_rate <- org_c[as.character(fert_long$fert_id), 3]

  fert_long <- fert_long %>%
    dplyr::mutate(c_added = (c_rate / 100 * Application_rate) / 1000) %>%
    dplyr::group_by(field_id) %>%
    dplyr::summarize(add_fym_field = sum(c_added), n = dplyr::n()) %>%
    dplyr::ungroup()

  field_data <- dplyr::left_join(field_data, fert_long, "field_id")
  return(field_data)
}
