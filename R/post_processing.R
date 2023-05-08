#'
#' @export
post_processing <- function(out, conversion = 44/12){
  s_ly <- out$soilC_scenario[(nrow(out$soilC_scenario) - 11):nrow(out$soilC_scenario),] |> rowSums() |> mean()
  b_ly <- out$soilC_baseline[(nrow(out$soilC_baseline) - 11):nrow(out$soilC_baseline),] |> rowSums() |> mean()
  year_intpl <- (s_ly - b_ly) / (nrow(out$soilC_scenario)/12)


  s_fy <- out$soilC_scenario[1:12,] |> rowSums() |> mean()
  b_fy <- out$soilC_baseline[1:12,] |> rowSums() |> mean()
  year <- (s_fy - b_fy)

  year_size <- 12
  years <- ceiling(nrow(out$soilC_baseline) / year_size)
  year_mean <- NULL
  for (i in 1:years) {
    start <- (i - 1) * year_size + 1
    end <- min(nrow(out$soilC_baseline), i * year_size)
    year_mean[i] <- out$soilC_scenario[start:end,] |> rowSums() |> mean()
  }
  out <- data.frame("year_intpl"=year_intpl*conversion, "first_year" = year*conversion)
  attr(out, "all_years") <- year_mean * conversion
  return(out)
}
