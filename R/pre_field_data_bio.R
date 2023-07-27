#' @import dplyr
#' @export

pre_field_data_bio <- function(field_data) {
  # ==========================
  # Selecting relevant columns
  # ==========================

  sel_col <- c(
    "user_id",
    "field_id",
    "field_size_ha",
    "field_longitude",
    "field_latitude",
    "field_def_country",
    "actual_crop_name",
    "actual_crop_type_id",
    "field_def_soil_texture",
    "field_def_crop_gross_yield",
    "field_def_fertilisers_mixed",
    "actual_crop_net_yield",
    "actual_crop_gross_yield",
    "field_def_crop_net_yield",
    "field_def_avg_temperature",
    "field_def_soil_som_specific",
    "field_def_cover_crops_adoption",
    "actual_cover_crops_adoption",
    "field_def_tilling",
    "actual_tilling",
    "field_def_crop_residue_management_name_short",
    "actual_crop_residue_management_name_short",
    "calc_baseline_cft_emission_removals_ton_co2e",
    "calc_actual_cft_emission_removals_ton_co2e",
    "actual_fertilisers_mixed",
    "add_fym_field"
  )

  field_data <- field_data[, sel_col]

  data_source <- getOption("data_source")
  # crop_names <- read.csv(file.path(data_source, getOption("crop_names")))[, c(3, 4)]
  crop_names <- load_crop_names()
  field_data <- field_data %>%
    mutate(actual_crop_name = crop_names[match(actual_crop_name, crop_names[, 1]), 2]) %>%
    drop_na(actual_crop_name) %>% # removing fields with NA actual crop names meaning they are fallow
    mutate(actual_residues = yield_to_resid2(actual_crop_gross_yield, actual_crop_name)) %>%
    mutate(add_bio_inpts = if_else(
      field_def_crop_residue_management_name_short %in% c("Removed", "Burned") &
        actual_crop_residue_management_name_short %in% c("Mulched"), 1,
      if_else(
        field_def_crop_residue_management_name_short %in% c("Mulched") &
          actual_crop_residue_management_name_short %in% c("Removed", "Burned"), -1,
        0
      )
    ) * actual_residues) %>%
    mutate(add_fym_inpts = if_else(
      field_def_fertilisers_mixed %in% c("only synthetic") &
        actual_fertilisers_mixed %in% c("Mixed", "Only organic"), 1,
      if_else(
        field_def_fertilisers_mixed %in% c("Mixed", "Only organic") &
          actual_fertilisers_mixed %in% c("only synthetic"), -1,
        0
      )
    ) * add_fym_field) %>%
    mutate(
      add_fym_inpts = if_else(is.na(add_fym_inpts), 0, add_fym_inpts),
      add_fym_field = if_else(is.na(add_fym_field), 0, add_fym_field)
    )

  return(field_data)
}
