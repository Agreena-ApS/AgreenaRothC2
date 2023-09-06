#' Preprocess field data for bioenergy analysis
#'
#' This function performs preprocessing on field data for bioenergy analysis,
#' including selecting relevant columns, joining data sources, and calculating
#' various metrics.
#'
#' @param field_data A data frame containing field-level data.
#'
#' @return A modified data frame with preprocessed field data.
#'
#' @details This function selects specific columns from the input field_data,
#' joins additional data sources, and calculates capped crop gross yields and
#' other metrics based on various conditions.
#'
#' @seealso \code{\link{load_crop_names}}, \code{\link{load_base_yields}},
#' \code{\link{yield_to_resid2}}
#'
#' @import dplyr
#'
#' @examples
#' # Load sample field data
#' data(field_data)
#' 
#' # Preprocess the field data for bioenergy analysis
#' preprocessed_data <- pre_field_data_bio(field_data)
#'
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
  base_yld <- load_base_yields() %>% select("Agreena_crop_IDENTIFIER","eurostat_country_code","yield_avg")

    # Calculate capped_crop_gross_yields
    field_data <- field_data %>%
    left_join(base_yld, by = c("actual_crop_name" = "Agreena_crop_IDENTIFIER","field_def_country" = "eurostat_country_code")) %>%
    mutate(capped_crop_gross_yields = ifelse(actual_crop_gross_yield > yield_avg * 1.2, yield_avg * 1.2, actual_crop_gross_yield)) %>% 
      mutate(capped_crop_gross_yields = ifelse(is.na(capped_crop_gross_yields),actual_crop_gross_yield, capped_crop_gross_yields)) %>% 
      mutate(actual_crop_gross_yield = capped_crop_gross_yields) %>% 
      select(-capped_crop_gross_yields)
  
    # Crops with no correspondence on the baseline table at the moment
    # 2 SEED-GRASS      
    # 3 SOYABEAN        
    # 4 FEED-GRASS      
    # 5 MILLET          
    # 6 CLOVER          
    # 7 OTHER-LEGUME    
    # 8 QUINOA          
    # 9 TREE-CROP       
    # 10 MUSTARD         
    # 11 TRITICALE       
    # 12 SORGHUM 
    
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
