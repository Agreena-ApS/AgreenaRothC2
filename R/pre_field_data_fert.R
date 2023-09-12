#' Process Field and Fertilizer Data
#'
#' This function reads field and fertilizer data from CSV or Excel files, calculates
#' the nitrogen and carbon contributions from fertilizers, and joins the data into
#' a single dataframe.
#'
#' @param actuals_year The year for which the data should be processed.
#' @param N_limit The nitrogen limit for scaling application rates (default is 300).
#'
#' @return A dataframe containing processed field and fertilizer data.
#'
#' @details This function performs the following steps:
#'   1. Determines the file delimiter for CSV files.
#'   2. Reads field and fertilizer data from specified files.
#'   3. Converts character columns in field_data to numeric if possible.
#'   4. Reads organic carbon content rates from a CSV file.
#'   5. Filters and selects relevant columns from the fertilizer data.
#'   6. Computes nitrogen rates based on fertilizer data and organic nitrogen content.
#'   7. Scales application rates to stay within the nitrogen limit (N_limit).
#'   8. Computes carbon rates based on scaled application rates.
#'   9. Joins the processed data into a single dataframe.
#'
#' @examples
#' \dontrun{
#'   pre_field_data_fert(2022)
#' }
#'
#' @importFrom readr 
#' @importFrom readxl 
#' @importFrom dplyr 
#'
#' @export

pre_field_data_fert <- function(actuals_year, N_limit = 300) {
  get_file_delimiter <- function(file_path) {
    file_extension <- tools::file_ext(file_path)

    if (file_extension == "csv") {
      first_line <- read_lines(file_path, n_max = 1)
      if (grepl(",", first_line)) {
        delimiter <- ","
      } else if (grepl(";", first_line)) {
        delimiter <- ";"
      } else {
        delimiter <- ","
        cat("No delimiter detected. Assuming comma (',') as the default delimiter.\n")
      }
    } else if (file_extension == "xlsx") {
      delimiter <- ","
      cat("Opening an Excel file. Assuming comma (',') as the default delimiter.\n")
    } else {
      stop("Unsupported file format. Only .csv and .xlsx files are supported.")
    }

    delimiter
  }

  delimiter1 <- get_file_delimiter(getOption("field_data"))
  delimiter2 <- get_file_delimiter(getOption("fert_data"))

  file_extension1 <- tools::file_ext(getOption("field_data"))
  file_extension2 <- tools::file_ext(getOption("fert_data"))

  if (file_extension1 == "csv") {
    field_data <- read_delim(getOption("field_data"), delim = delimiter1)
  } else if (file_extension1 == "xlsx") {
    field_data <- readxl::read_xlsx(getOption("field_data"))
  } else {
    stop("Unsupported file format. Only .csv and .xlsx files are supported.")
  }

  if (file_extension2 == "csv") {
    fert_data <- read_delim(getOption("fert_data"), delim = delimiter2)
  } else if (file_extension2 == "xlsx") {
    fert_data <- readxl::read_xlsx(getOption("fert_data"))
  } else {
    stop("Unsupported file format. Only .csv and .xlsx files are supported.")
  }
  
  field_data <- field_data %>%
    mutate_if(is.character, function(x) ifelse(is.na(as.numeric(x)), x, as.numeric(x)))

  data_source <- getOption("data_source")
  
  org_c <- read.csv(file.path(data_source, getOption("org_fert_c_rate")))
  rownames(org_c) <- org_c[, 2]
  fert_data <- fert_data %>%
    mutate(actual_harvest_year = if_else(is.na(actual_harvest_year), 9999, as.numeric(actual_harvest_year))) %>%
    # dplyr::filter(actual_harvest_year == actuals_year, actual_fertilisers_mixed != "Only synthetic") %>%
    dplyr::filter(actual_harvest_year == actuals_year) %>%
    select(
      "field_id","actual_fertilisers_mixed", contains("actual_fertiliser_id_"),
      contains("fertiliser_actual_application_rate"),
      contains("fertiliser_actual_fertiliser_name"),
      contains("fertiliser_actual_mode"),
      contains("fertiliser_actual_nitrogen_kg_ha")
    )
  
  # testing what happens where there is an Synt fertlizer in the mix
  # fert_data[fert_data$field_id == 43333,"fertiliser_actual_fertiliser_id_07" ] = 1
  # fert_data[fert_data$field_id == 43333,"fertiliser_actual_fertiliser_name_07" ] = "Amonia"
  # fert_data[fert_data$field_id == 43333,"fertiliser_actual_application_rate_07" ] = 400
  
  fert_names <- pivot_longer(fert_data,
                             cols = contains("fertiliser_actual_fertiliser_name_0"),
                             names_to = letters[1:5],
                             names_sep = "_",
                             values_to = "name"
  ) %>% select("field_id", "e", "name")
  
  fert_modes <- pivot_longer(fert_data,
                             cols = contains("fertiliser_actual_mode"),
                             names_to = letters[2:5],
                             names_sep = "_",
                             values_to = "mode"
  ) %>% select("field_id", "e", "mode")
  
  fert_rates <- pivot_longer(fert_data,
                             cols = contains("fertiliser_actual_application_rate_0"),
                             names_to = letters[1:5],
                             names_sep = "_",
                             values_to = "Application_rate"
  ) %>%
    select("field_id", "e", "Application_rate")
  
  fert_Nrates <- pivot_longer(fert_data,
                              cols = contains("fertiliser_actual_nitrogen"),
                              names_to = letters[1:6],
                              names_sep = "_",
                              values_to = "N_application"
  ) %>%
    select("field_id", "f", "N_application")
  
  fert_id <- pivot_longer(fert_data,
                          cols = contains("fertiliser_actual_fertiliser_id"),
                          names_to = letters[1:5],
                          names_sep = "_",
                          values_to = "fert_id"
  ) %>%
    select("field_id", "e", "fert_id")
  
  fert_long <- left_join(fert_id, fert_rates, by = c("field_id", "e")) %>%
    left_join(fert_names, by = c("field_id", "e")) %>%
    left_join(fert_modes, by = c("field_id", "e")) %>%
    left_join(fert_Nrates, by = c("field_id", "e" = "f")) %>%
    drop_na(mode)
  
  fert_long$n_rate <- fert_long %>%
    left_join(org_c, by = c("fert_id" = "id")) %>%
    mutate(nitro_content = ifelse(is.na(nitro_content), 100, nitro_content)) %>% 
    pull(nitro_content)
  

# Parei aqui, continuar segunda abrir aba fert_long
  fert_long <- fert_long %>% mutate(name = ifelse(mode == "synthetic","synthetic", name), 
                                    Application_rate = ifelse(mode == "synthetic", N_application,Application_rate))
  
  
#=================
# Recalculate the application rates based on N content
  new_application_rates <- fert_long %>%
    mutate(add_N_field = ((n_rate / 100) * Application_rate)) %>%
    # group_by(field_id, fert_id, e) %>%
    # summarize(add_N_field = sum(n_added), Application_rate = Application_rate, n = n()) %>%
    group_by(field_id) %>%
    mutate(total_add_N_field = sum(add_N_field)) %>%
    mutate(share_contribution = add_N_field / total_add_N_field) %>%
    mutate(excess_diff = ifelse(total_add_N_field > N_limit, total_add_N_field - N_limit, 0)) %>%
    mutate(scaling_factor = ifelse(excess_diff > 0, (total_add_N_field - excess_diff) / total_add_N_field, 1)) %>%
    # mutate(Application_rate_scaled = ifelse(total_add_N_field > N_limit, Application_rate * scaling_factor, Application_rate)) %>%
    mutate(Application_rate_scaled = Application_rate * scaling_factor)
#=================
  
  fert_long <- left_join(fert_long, new_application_rates)
  
  fert_long$c_rate <- fert_long %>%
    left_join(org_c, by = c("fert_id" = "id")) %>%
    mutate(carbon_content = ifelse(is.na(carbon_content), 0, carbon_content)) %>% 
    pull(carbon_content)

  fert_long <- fert_long %>%
    mutate(c_added = ((c_rate / 100) * Application_rate_scaled) / 1000) %>%
    group_by(field_id) %>%
    summarize(add_fym_field = sum(c_added), n = n()) %>%
    ungroup()

  field_data <- left_join(field_data, fert_long, "field_id")
  return(field_data)
}
