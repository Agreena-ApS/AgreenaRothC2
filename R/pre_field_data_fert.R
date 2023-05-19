
#' @import dplyr
#' @import tidyr
#' @import readr
#' @export

pre_field_data_fert <- function(actuals_year){

  field_data <- read_csv(getOption("field_data"))
  fert_data <- read_csv(getOption("fert_data"))
  data_source <- getOption("data_source")

  org_c <- read.csv(file.path(data_source,  getOption("org_fert_c_rate")))
  rownames(org_c) <- org_c[, 2]
  fert_data <- fert_data %>%
    mutate(actual_harvest_year = if_else(is.na(actual_harvest_year), 9999, as.numeric(actual_harvest_year))) %>%
    dplyr::filter(actual_harvest_year == actuals_year, actual_fertilisers_mixed != "Only synthetic") %>%
    select("field_id", contains("actual_fertiliser_id_"),
           contains("fertiliser_actual_application_rate"),
           contains("fertiliser_actual_fertiliser_name"))
  fert_names <- pivot_longer(fert_data,
                             cols = contains("fertiliser_actual_fertiliser_name_0"),
                             names_to = letters[1:5],
                             names_sep = "_",
                             values_to = "name") %>%
    select("field_id", "e","name")
  fert_rates <- pivot_longer(fert_data,
                             cols = contains("fertiliser_actual_application_rate_0"),
                             names_to = letters[1:5],
                             names_sep = "_",
                             values_to = "Application_rate") %>%
    select("field_id", "e", "Application_rate")
  fert_id <- pivot_longer(fert_data,
                          cols = contains("fertiliser_actual_fertiliser_id"),
                          names_to = letters[1:5],
                          names_sep = "_",
                          values_to = "fert_id") %>%
    select("field_id", "e", "fert_id")
  fert_long <- left_join(fert_id, fert_rates, by = c("field_id", "e")) %>%
    left_join(fert_names, by = c("field_id", "e")) %>% drop_na()

  fert_long$c_rate <- org_c[as.character(fert_long$fert_id), 3]

  fert_long <- fert_long %>%
    mutate(c_added = (c_rate/100 * Application_rate) / 1000) %>%
    group_by(field_id) %>%
    summarize(add_fym_field = sum(c_added), n = n()) %>%
    ungroup()

  field_data <- left_join(field_data, fert_long, "field_id")
  return(field_data)
}
