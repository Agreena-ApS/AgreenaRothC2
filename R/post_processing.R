#'
#' @export
post_processing <- function(out, type = "payout", conversion = 44 / 12) {

    # s_ly <- out$soilC_scenario[(nrow(out$soilC_scenario) - 11):nrow(out$soilC_scenario), ] |>
    #   rowSums() |>
    #   mean()
    # b_ly <- out$soilC_baseline[(nrow(out$soilC_baseline) - 11):nrow(out$soilC_baseline), ] |>
    #   rowSums() |>
    #   mean()
    # year_intpl <- (s_ly - b_ly) / (nrow(out$soilC_scenario) / 12)

    s_ly <- out$soilC_scenario[nrow(out$soilC_scenario), ] - out$soilC_scenario[1, ]
    s_ly <- sum(s_ly)
    
    b_ly <- out$soilC_baselineout[nrow(out$soilC_baselineout), ] - out$soilC_baselineout[1, ]
    b_ly <- sum(b_ly)
    year_intpl <- (s_ly - b_ly) / (nrow(out$soilC_scenario) / 12)
  
    # s_fy <- out$soilC_scenario[1:12, ] |>
    #   rowSums() |>
    #   mean()
    # b_fy <- out$soilC_baseline[1:12, ] |>
    #   rowSums() |>
    #   mean()
    # year <- (s_fy - b_fy)

    year_size <- 12
    years <- ceiling(nrow(out$soilC_baseline) / year_size)
    year_mean <- NULL
    year_mean_s <- NULL
    year_mean_b <- NULL
    year_mean_last_month <- NULL
    year_mean_12m_avg <- NULL
    # for (i in 1:years) {
    #   start <- (i - 1) * year_size + 1
    #   end <- min(nrow(out$soilC_baseline), i * year_size)
    #   year_mean_s[i] <- out$soilC_scenario[start:end,] |> rowSums() |> mean()
    #   year_mean_b[i] <- out$soilC_baseline[start:end,] |> rowSums() |> mean()
    #   if(i == 1) {year_mean[i] <- year_mean_s[i] - year_mean_b[i]} else { year_mean[i] <- year_mean_s[i] - year_mean_s[i-1] }
    # }

    # Approach to calculate project development that considers SOC in December and is compliant with the first_year payout approach

    for (i in 1:years) {
      start <- (i - 1) * year_size + 1
      end <- min(nrow(out$soilC_baseline), i * year_size)
      year_mean_s[i] <- out$soilC_scenario[end, ] |> sum()
      year_mean_b[i] <- out$soilC_baseline[end, ] |> sum()
      if (i == 1) {
        year_mean_last_month[i] <- year_mean_s[i] - year_mean_b[i]
      } else {
        year_mean_last_month[i] <- year_mean_s[i] - year_mean_s[i - 1]
      }
    }
    

    for (i in 1:years) {
      start <- (i - 1) * year_size + 1
      end <- min(nrow(out$soilC_baseline), i * year_size)
      year_mean_s[i] <- out$soilC_scenario[start:end, ] |> rowSums() |> mean()
      year_mean_b[i] <- out$soilC_baseline[start:end, ] |> rowSums() |> mean()
      if (i == 1) {
        year_mean_12m_avg[i] <- year_mean_s[i] - year_mean_b[i]
      } else {
        year_mean_12m_avg[i] <- year_mean_s[i] - year_mean_s[i - 1]
      }
    }
    
    out <- list(
         "all_years_last_month" = year_mean_last_month * conversion,
         "all_years_12m_avg" = year_mean_12m_avg * conversion,
         "year_intpl" = year_intpl * conversion,
         "n_years" = 1:years
         )
    
    # out <- data.frame("year_intpl" = year_intpl * conversion, "first_year" = year * conversion)
    # attr(out, "all_years") <- year_mean * conversion
    # attr(out, "n_years") <- 1:years
    return(out)

}
