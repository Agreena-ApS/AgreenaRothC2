#' Calculate emissions reductions and removals certificates
#'
#' This function takes a list of files containing RothC model outputs, IPCC model outputs, and field data,
#' and calculates the emissions reductions and removals certificates for each field. It also applies
#' uncertainty deduction, buffer pool deduction, fees deduction, and premium payment to the certificates.
#'
#' @param files A named list of files containing RothC model outputs, IPCC model outputs, and field data.
#' @param uncertainty_rem A numeric value for the uncertainty deduction factor for removals (default is 0.15).
#' @param uncertainty_red A numeric value for the uncertainty deduction for emissions reductions (default is 0)
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
                            uncertainty_rem = 0.15,
                            uncertainty_red = 0,
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
  merged[is.na(merged)] <- 0
  # Calculate the emissions reductions and removals for each field
  joined_calc_mutate <- merged %>%
    mutate(
      # Fuel emissions reductions (tCO2e/ha)
      delta_fuel_emissions = baseline_fuel_emissions - actual_fuel_emissions,

      # Soil N2O emissions reductions (tCO2e/ha)
      delta_soil_n2o_emissions  = baseline_soil_n2o_emissions - actual_soil_n2o_emissions,

      # Uncertainty deduction factor for removals
      uncertainty_deduction_rem = uncertainty_rem,
      
      # Uncertainty deduction factor for emissions reductions
      uncertainty_deduction_red = uncertainty_red,

      # Total emissions reductions (tCO2e)
      Ered_eq37_field = (delta_fuel_emissions + delta_soil_n2o_emissions * (1 - uncertainty_deduction_red)) * field_size_ha,

      # Total emissions removals (tCO2e)
      Erem_eq38_field = (co2_removals - total_leakage_tC_ha) * (1 - uncertainty_deduction_rem) * field_size_ha,

      # Estimated net GHG emissions reductions and removals
      ERR_eq39_field = Ered_eq37_field + Erem_eq38_field,
      
      # Buffer pool deduction (x% of total certificates) (tCO2e)
      buffer_field = Erem_eq38_field * buffer,
      
      #Number of VCUs in year
      VCU_field_eq66 = Ered_eq37_field + (Erem_eq38_field - buffer_field),
      
      # Fees deduction (x% of total certificates) (tCO2e)
      fees = VCU_field_eq66 * fees,

      # Net certificates after deductions (tCO2e)
      net_certs = VCU_field_eq66 - fees,

      # Net certificates per hectare after deductions (tCO2e/ha)
      net_certs_ha = net_certs / field_size_ha,

      # Premium payment for net certificates (x% of net certificates) ($)
      premium = net_certs * premium
    )

  # Add an attribute to the data frame with Fallow_fields
  attr(joined_calc_mutate, "Fallow_fields") <- Fallow_fields

  return(joined_calc_mutate)
}
