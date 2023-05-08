#' @title Environmental Variables
#' @description Download and preprocess management variables.
#' @name management_variables
#' @param mgmt_file google sheets with field data
#' @param write write output of management, only used for debugging!
#' @param flag separete treatment from control fields
#' @param cf1 Coefficient factor for slope yield inputs
#' @param cf2 Coefficient factor Intercept yield inputs
#' @param cf3 Coefficient factor DR ratio
#' @author Marcos Alves
#' @import stringr digest
#' @export
#'

management_variables <- function(mgmt_file, flag = c("Treatment", "Control"), write = FALSE,  cf1 = 1, cf2 = 1, cf3 = 1) {
  if (!dir.exists("inputs")) {
    dir.create("inputs")
  }

  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }

  # files <- list.files("inputs")
  id <- substr(digest::sha1(mgmt_file),1,10)
  file_name <- paste0(mgmt_file$Latitude, "_", mgmt_file$Longitude, "_", mgmt_file$Field_ID, "_", "mgmt_", flag, "_", id, ".csv")
  # if (any(grepl(file_name, files)) & use_cache) {
  #   res <- read.csv(paste0(getwd(), "/inputs/", file_name))
  #   return(res)
  # }

  years <-
    ((mgmt_file[, "Study_period_end"] - mgmt_file[, "Study_period_start"]) / 365)[[1]] %>%
    round() %>%
    as.numeric()
  sim_period <- seq(1 / 12, years, by = 1 / 12)

  cropsdf <- data.frame("t" = sim_period)

  # crops
  crops <- mgmt_file[, "Crops_Rotations"] %>%
    str_split(",") %>%
    unlist() %>%
    trimws(which = "both")
  if (years < length(crops)) stop("Study period shorter than number of crop rotations")
  cropsdf$crop <- rep_len(rep(crops, each = 12), length.out = length(sim_period))

  # residue management
  res_mgmt <- mgmt_file[, "Residue_Management"] %>%
    str_split(",") %>%
    unlist()
  if (length(crops) != length(res_mgmt) & length(res_mgmt) > 1) stop("Number of crops and residues management do not match")
  crop_residue <- residue_management(res_mgmt)
  cropsdf$cover <- rep_len(crop_residue, length.out = length(sim_period))

  # country id
  country <- mgmt_file[, "Country_Code"] %>%
    unlist() %>%
    str_split(",") %>%
    unlist()
  cropsdf$country <- rep_len(country, length.out = length(sim_period))

  # Yields
  yields <- mgmt_file[, "NCY_converted_value"] %>%
    unlist() %>%
    str_split(",") %>%
    unlist() %>%
    as.numeric() %>% suppressWarnings()
  if (length(yields) != length(crops)) stop("Number of yields and crops are different")
  cropsdf$annual_yield <- rep_len(rep(yields, each = 12), length.out = length(sim_period))


  # avg_yld <- readxl::read_xlsx("/Users/marcospaulopedrosaalves/Documents/Git/AgreenaRothC_data/Yield_per_ha_estimated_datasets_final.xlsx")
  # avg_yld <- readxl::read_xlsx("/Users/marcospaulopedrosaalves/Library/CloudStorage/GoogleDrive-marcos.alves@agreena.com/Shared drives/Agreena all/09 Product, Program & Science/03 Product/03 Programme Team/01_CA Programme/02_CA_Methodology_V.2/Leakage/Preparing yield data_eurostat_fao/Yield_per_ha_estimated_datasets_2.xlsx")
  # avg_yld$Agreena_crop_name <- as.character(toupper(avg_yld$Agreena_crop_IDENTIFIER))
  # # write.csv2(unique(as.character(avg_yld$Agreena_crop_IDENTIFIER))[order(unique(as.character(avg_yld$Agreena_crop_IDENTIFIER)))], "list_crops.csv")
  # usethis::use_data(avg_yld, overwrite = T)

  nas <- is.na(cropsdf$annual_yield)
  joint <- left_join(cropsdf, avg_yld[, c("eurostat_country_code", "Agreena_crop_IDENTIFIER", "yield_avg")], by = c("country" = "eurostat_country_code", "crop" = "Agreena_crop_IDENTIFIER"))
  cropsdf[nas, "annual_yield"] <- joint[nas, "yield_avg"]

  # Manure
  fym <- mgmt_file[, "Manure_C_value"] %>%
    unlist() %>%
    str_split(",") %>%
    unlist() %>%
    as.numeric() %>% suppressWarnings()
    if(any(is.na(fym))){ fym <- rep(0, length(fym)) } else { fym <- fym }
    if(is.null(fym)){ fym <- rep(0, length(fym)) } else { fym <- fym }

  if (length(yields) != length(fym)) stop("Number of yields and fym are different")
  cropsdf$fym <- rep_len(rep(fym, each = 12), length.out = length(sim_period))

  #from Agreena to CFT crops
  crops <- agreena2cft(crops)

  # biomass distribution over the year
  res_fraction <- residue_fractions(crops)
  cropsdf$biomass_distrib <- rep_len(res_fraction, length.out = length(sim_period))

  # biomass input estimation
  residues <- yield_to_resid(cropsdf$annual_yield, cropsdf$crop, cf1 = cf1, cf2 = cf2)
  bio_mass_input <- mgmt_file[, "BI_Converted_Value"] %>%
    unlist() %>%
    str_split(",") %>%
    unlist() %>%
    as.numeric() %>% suppressWarnings()
    if(any(is.na(bio_mass_input))){ bio_mass_input <- rep(0, length(bio_mass_input)) } else { bio_mass_input <- bio_mass_input }
    if(is.null(bio_mass_input)){ bio_mass_input <- rep(0, length(bio_mass_input)) } else { bio_mass_input <- bio_mass_input }

  if (length(crops) != length(bio_mass_input)) stop("Number of crops and biomass inputs are different")
  cropsdf$bio_mass_input <- rep_len(rep(bio_mass_input, each = 12), length.out = length(sim_period))

  cropsdf$residues <- cropsdf$bio_mass_input
  cropsdf$residues[!as.logical(cropsdf$bio_mass_input)] <- residues[!as.logical(cropsdf$bio_mass_input)] * cropsdf$biomass_distrib[!as.logical(cropsdf$bio_mass_input)]
  cropsdf$residues[as.logical(cropsdf$bio_mass_input)] <- cropsdf$bio_mass_input[as.logical(cropsdf$bio_mass_input)] * cropsdf$biomass_distrib[as.logical(cropsdf$bio_mass_input)]
  # Soil Cover
  cover_crops <- mgmt_file[, "Cover_crop_regime"] %>%
    str_split(",") %>%
    unlist()
  cover <- cover_crop_convertion(cover_crops)
  cropsdf$cc <- rep_len(cover, length.out = length(sim_period))
  cropsdf$cc_name <- rep_len(rep(cover_crops, each = 12), length.out = length(sim_period))

  # Tillage regime
  tillage_cr <- mgmt_file[, "Tillage_regime"] %>%
    str_split(",") %>%
    unlist()
  till_effect <- tillage_convertion(tillage_cr)
  if (length(crops) != length(tillage_cr) & length(tillage_cr) > 1) stop("Number of crops and tillage management are not compatible")
  cropsdf$tillage_cr <- rep_len(rep(till_effect, each = 12), length.out = length(sim_period))

  # DPM/RPM ratio
  cropsdf$DR <- 1.44


  # Flagging
  flag <- match.arg(flag)
  cropsdf$flag <- flag

  # calibration factors
  cropsdf$cf1 <- cf1
  cropsdf$cf2 <- cf2
  cropsdf$cf3 <- cf3

  if(write == TRUE) {write.csv(cropsdf, paste0("inputs/", file_name))}

  return(cropsdf)
}



# mgmt <- management_variables(sheet[1,],"Treatment")


# yld2bio[c("SPRING WHEAT"), c("Slope.a", "Intercept.b")] <- c(1.29, 0.75)
# usethis::use_data(yld2bio, overwrite = T)
# # creasting the databases with assumptions
#
# resid <- t(data.frame(
#   "Mulched" = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
#   "Removed" = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
# ))
#
# colnames(resid) <- month.name
# # write.csv(resid, "/Users/marcospaulopedrosaalves/Documents/Git/AgreenaRothC/data/resid.csv")
# # usethis::use_data(resid, overwrite = T)
#
# cc <- t(data.frame(
#   "winter" = c(1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1),
#   "none" = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0),
#   "spring" = c(0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0),
#   "catch" = c(0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
# ))
#
# colnames(cc) <- month.name
# # write.csv(cc, "/Users/marcospaulopedrosaalves/Documents/Git/AgreenaRothC/data/cc.csv")
# # usethis::use_data(cc)
#
#
# res_fraction <- yld2bio
# for (i in month.name) {
#   res_fraction <- add_column(res_fraction,i = 0)
# }
# res_fraction[] <- 0
# res_fractions <- res_fraction[,1:12]
# colnames(res_fractions) <- month.name
# res_fractions[,9] <- 1
# write.csv(res_fractions, "/Users/marcospaulopedrosaalves/Documents/Git/AgreenaRothC/data/res_fractions.csv")
# usethis::use_data(res_fractions, overwrite = T)
#
# tillage_convert <- t(data.frame("Conventional" = 1,
#             "Notill" = 0.95,
#             "Reducedtill" = 0.93,
#             "Notavailable" = 1))
# # write.csv(tillage_convert, "/Users/marcospaulopedrosaalves/Documents/Git/AgreenaRothC/data/tillage_convert.csv")
# # usethis::use_data(tillage_convert, overwrite = T)



