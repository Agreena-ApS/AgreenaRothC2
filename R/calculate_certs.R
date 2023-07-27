#' Calculate emissions reductions and removals certificates
#'
#' This function takes a list of files containing RothC model outputs, IPCC model outputs, and field data,
#' and calculates the emissions reductions and removals certificates for each field. It also applies
#' uncertainty deduction, buffer pool deduction, fees deduction, and premium payment to the certificates.
#'
#' @param files A named list of files containing RothC model outputs, IPCC model outputs, and field data.
#' @param uncertain_uncertanty_deduction A numeric value for the uncertainty deduction factor (default is 0.0896).
#' @param fees A numeric value for the fees deduction percentage (default is 0.15).
#' @param buffer A numeric value for the buffer pool deduction percentage (default is 0.2).
#' @param premium A numeric value for the premium payment percentage (default is 0.1).
#' @param add_fallow_fields A logical value indicating whether to add an attribute to the data frame with the field_id of rows with NA in actual_crop_name (default is TRUE).
#'
#' @return A data frame of emissions reductions and removals certificates for each field, with an optional attribute of fallow fields.
#'
#' @examples
#' # Create an example list of files
#' files <- list(
#'   "RothC" = list("Sumarized_outputs_Batch_1_v3.rds", "Sumarized_outputs_Batch_2_v3.rds"),
#'   "ipcc" = list("result_ipcc_batch_1.csv", "result_ipcc_batch_2.csv"),
#'   "field_data" = list("field_data_Batch_2_v3.rds", "field_data_Batch_1_v3.rds")
#' )
#'
#' # Call the function and print the result
#' result <- calculate_certs(files)
#' print(result)
#'
#' @import dplyr
#' @export

calculate_certs <- function(files, uncertain_uncertanty_deduction = 0.0896,
                            fees = 0.15,
                            buffer = 0.2,
                            premium = 0.1,
                            add_fallow_fields = TRUE) {
  # Call the bind_and_merge function to get the merged data
  merged <- bind_and_merge(files)
  
  # backward compatibility
  if (exists("first_year", where = merged)) {
    names(merged)[grep("first_year", names(merged))] <- "co2_removals"
  }
  
  Fallow_fields <- NULL

  if (!add_fallow_fields) {
    # Store the field_id of rows with NA in actual_crop_name
    Fallow_fields <- merged$field_id[is.na(merged$actual_crop_name)]

    # Remove rows with NA in actual_crop_name
    merged <- na.omit(merged, "actual_crop_name")
  }

  # Calculate the emissions reductions and removals for each field
  joined_calc_mutate <- merged %>%
    mutate(
      # Fuel emissions reductions (tCO2e/ha)
      fuel_reductions = baseline_fuel_emissions - actual_fuel_emissions,

      # Soil N2O emissions reductions (tCO2e/ha)
      soil_n2o_reductions = baseline_soil_n2o_emissions - actual_soil_n2o_emissions,

      # Fertilizer N2O emissions reductions (tCO2e/ha)
      fert_reductions = baseline_fertilizer_n2o_emissions - actual_fertilizer_n2o_emissions,

      # Manure N2O emissions reductions (tCO2e/ha)
      manure_reduction = baseline_manure_n2o_emissions - actual_manure_n2o_emissions,

      # Nitrogen fixing emissions reductions (tCO2e/ha)
      n_fixing = baseline_nitrogen_fixing_emissions - actual_nitrogen_fixing_emissions,

      # Uncertainty deduction factor
      uncertain_uncertanty_deduction = 0.0896,

      # Total emissions reductions (tCO2e/ha)
      total_reductions_tCha = fuel_reductions +
        soil_n2o_reductions,

      # Total emissions removals (tCO2e/ha)
      total_removals_tCha = co2_removals,

      # Total emissions reductions (tCO2e)
      total_reductions_area = total_reductions_tCha * field_size_ha,

      # Total emissions removals (tCO2e)
      total_removals_area = total_removals_tCha * field_size_ha,

      # Emissions removals minus uncertainty deduction (tCO2e/ha)
      removals_minus_uncertainty_tCha = total_removals_tCha * (1 - uncertain_uncertanty_deduction),

      # Emissions reductions minus uncertainty deduction (tCO2e/ha)
      reductions_minus_uncertainty_tCha = total_reductions_tCha * (1 - uncertain_uncertanty_deduction),

      # Emissions removals minus uncertainty deduction (tCO2e)
      removals_minus_uncertainty_area = total_removals_area * (1 - uncertain_uncertanty_deduction),

      # Emissions reductions minus uncertainty deduction (tCO2e)
      reductions_minus_uncertainty_area = total_reductions_area * (1 - uncertain_uncertanty_deduction),

      # Total certificates (tCO2e)
      total_certs = reductions_minus_uncertainty_area + removals_minus_uncertainty_area,

      # Total certificates per hectare (tCO2e/ha)
      total_certs_ha = total_certs / field_size_ha,

      # Buffer pool deduction (x% of total certificates) (tCO2e)
      buffer = total_certs * buffer,

      # Fees deduction (x% of total certificates) (tCO2e)
      fees = total_certs * fees,

      # Net certificates after deductions (tCO2e)
      net_certs = total_certs - buffer - fees,

      # Net certificates per hectare after deductions (tCO2e/ha)
      net_certs_ha = net_certs / field_size_ha,

      # Premium payment for net certificates (x% of net certificates) ($)
      premium = net_certs * premium
    )

  # Add an attribute to the data frame with Fallow_fields
  attr(joined_calc_mutate, "Fallow_fields") <- Fallow_fields

  return(joined_calc_mutate)
}
