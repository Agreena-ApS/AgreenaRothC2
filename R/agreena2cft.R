agreena2cft <- function(crops) {
  crop_names <- load_crop_names()
  crop_names$coolfarm_name <- toupper(crop_names$coolfarm_name)
  crop_names$Agreena_crop_IDENTIFIER <- toupper(crop_names$Agreena_crop_IDENTIFIER)
  cropname <- trimws(crops, which = "both") %>% toupper()


  cropcft <- crop_names[match(crops, crop_names$Agreena_crop_IDENTIFIER), "coolfarm_name"]

  # crops_pos <- which(crop_names$Agreena_crop_name %in% cropname)
  # cropcft <- crop_names[crops_pos,"coolfarm_name"]
  return(unlist(cropcft))
}


# crop_names <- readxl::read_xlsx("/Users/marcospaulopedrosaalves/Library/CloudStorage/GoogleDrive-marcos.alves@agreena.com/Shared drives/Agreena all/09 Product, Program & Science/03 Product/03 Programme Team/01_CA Programme/02_CA_Methodology_V.2/Leakage/Preparing yield data_eurostat_fao/Matching crop names.xlsx")[,c("Agreena_crop_IDENTIFIER", "coolfarm_name")]
# # usethis::use_data(crop_names, overwrite = T)
#
# write.csv(crop_names,"crop_names.csv", row.names = F)
# read.csv("crop_names.csv")
