#' @export
run_rothc_with_pythonlike_api <- function(
  field_lon,
  field_lat,
  monthly_temp,
  monthly_precip,
  monthly_evapotrans,
  n_years_to_solve = 20,
  ode_solve_step = 1 / 12,
  # now we have some baseline values that we should/could alter,
  decomp_rates = c(10, 0.3, 0.66, 0.02, 0),
  initial_carbon_stock_state = c(0, 0, 0, 0, 2.7),
  soilgrids_organic_carbon_stock_limit = 200,
  input_carbon = 1.7, # tC/ha
  farmyard_manure = 0, # tC/ha -- scenario only?
  clay_percent = 23.4,
  soil_thickness_cm = 30,
  plant_material_ratio = 1.44,
  evaporation_coefficient = 0.75,
  cover_crop_type = "none",
  ode_solver = "euler"
) {
  stopifnot(
    cover_crop_type %in% c("spring", "none", "winter", "catch"),
    # generous European bounding box coordinates
    between(field_lon, -11.5, 41.5),
    between(field_lat, 35.0, 70.0)
  )

  crop_presence <- cover_crop_type_to_presence_vector(cover_crop_type)

  time_grid_solve <- seq(from = 1 / 12, to = n_years_to_solve, by = ode_solve_step)

  soil_data <- c(field_lon, field_lat) |>
    get_isric_soil_profile_rothc(statistic = "mean", find.location.name = FALSE)

  inorganic_matter_estimate <- soil_data |>
    get_inorganic_matter_estimate(limit = soilgrids_organic_carbon_stock_limit)

  clay_average <- mean(soil_data$ParticleSizeClay[1 : 3])

  # factors that effect flow rate of carbon
  flow_rate_crop_retainment <- fC_crop_retainment(crop_presence)
  flow_rate_temperature <- SoilR::fT.RothC(monthly_temp)
  flow_rate_water <- f_moist_rothc(
    pp = monthly_precip,
    et = monthly_evapotrans,
    s_thick = soil_thickness_cm,
    pclay = clay_average,
    pE = 1.0,
    soil_cover = crop_presence
  )

  xi_coeffs <- prod(c(flow_rate_crop_retainment, flow_rate_temperature, flow_rate_water))

  rothc_results <- SoilR::RothCModel(
    t = time_grid_solve,
    ks = decomp_rates,
    C0 = c(0, 0, 0, 0, inorganic_matter_estimate),
    In = input_carbon,
    clay = clay_average,
    xi = xi_coeffs,
    DR = plant_material_ratio
  ) |>
    SoilR::getC()

  return(rothc_results)
}

get_inorganic_matter_estimate <- function(soil_data, limit) {
  val <- min(soil_data$Carbon[1], limit)
  return(0.049 * val^(1.139))
}

cover_crop_type_to_presence_vector <- function(x) {
  switch(x,
    "spring" = c(0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    "none" =   c(0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0),
    "winter" = c(0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0),
    "catch" =  c(1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1)
  )
}