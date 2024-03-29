% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_certs.R
\name{calculate_certs}
\alias{calculate_certs}
\title{Calculate emissions reductions and removals certificates}
\usage{
calculate_certs(
  files,
  uncertain_uncertanty_deduction = 0.0896,
  fees = 0.15,
  buffer = 0.2,
  premium = 0.1,
  uncertainty_rem = 0.14538659,
  uncertainty_red = 0,
  add_fallow_fields = TRUE,
  accounting_variable
)
}
\arguments{
\item{files}{A named list of files containing RothC model outputs, IPCC model outputs, and field data.}

\item{fees}{A numeric value for the fees deduction percentage (default is 0.15).}

\item{buffer}{A numeric value for the buffer pool deduction percentage (default is 0.2).}

\item{premium}{A numeric value for the premium payment percentage (default is 0.1).}

\item{uncertainty_rem}{A numeric value for the uncertainty deduction factor for removals (default is 0.15).}

\item{uncertainty_red}{A numeric value for the uncertainty deduction for emissions reductions (default is 0)}

\item{add_fallow_fields}{A logical value indicating whether to add an attribute to the data frame with the field_id of rows with NA in actual_crop_name (default is TRUE).}
}
\value{
A data frame of emissions reductions and removals certificates for each field, with an optional attribute of fallow fields.
}
\description{
This function takes a list of files containing RothC model outputs, IPCC model outputs, and field data,
and calculates the emissions reductions and removals certificates for each field. It also applies
uncertainty deduction, buffer pool deduction, fees deduction, and premium payment to the certificates.
}
\examples{
# Create an example list of files
files <- list(
  "RothC" = list("Sumarized_outputs_Batch_1_v3.rds", "Sumarized_outputs_Batch_2_v3.rds"),
  "ipcc" = list("result_ipcc_batch_1.csv", "result_ipcc_batch_2.csv"),
  "field_data" = list("field_data_Batch_2_v3.rds", "field_data_Batch_1_v3.rds")
)

# Call the function and print the result
result <- calculate_certs(files)
print(result)

}
