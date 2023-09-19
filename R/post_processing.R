#'
#' @export


post_processing <-
  function(out,
           type = "payout",
           conversion = 44 / 12) {
    # s_ly <- out$soilC_scenario[nrow(out$soilC_scenario), ] - out$soilC_scenario[1, ]
    # s_ly <- sum(s_ly)
    #
    # b_ly <- out$soilC_baselineout[nrow(out$soilC_baselineout), ] - out$soilC_baselineout[1, ]
    # b_ly <- sum(b_ly)
    # year_intpl <- (s_ly - b_ly) / (nrow(out$soilC_scenario) / 12)
    
    calc_interpolations <- function(out, interp_interval) {
      monthly_values_s <-
        rowSums(out$soilC_scenario) # - rowSums(out$soilC_baseline)
      monthly_values_b <-
        rowSums(out$soilC_baseline) # - rowSums(out$soilC_baseline)
      results <- list()
      
      for (j in 1:length(interp_interval)) {
        interp_interval_months <- 12 * interp_interval[j]
        # Check if interp_interval[j] is a multiple of length(monthly_values_s) 
        if (length(monthly_values_s) %% interp_interval_months == 0) {
          num_intervals <-
            length(monthly_values_s) / interp_interval_months
          average_changes <- numeric(num_intervals)
          for (i in 1:num_intervals) {
            start_index <- (i - 1) * interp_interval_months + 1
            end_index <- i * interp_interval_months
            if (i == 1) {
              average_changes[i] <-
                (monthly_values_s[end_index] - monthly_values_b[end_index]) / interp_interval[j]  # Divide by the number of years
            } else {
              average_changes[i] <-
                (monthly_values_s[end_index] - monthly_values_s[start_index - 1]) / interp_interval[j]  # Divide by the number of years
            }
          }
          results[[paste0("Interval_", interp_interval[j])]] <-
            rep(average_changes, each = interp_interval[j])
        }
      }
      # Convert the list to a data frame and return it
      return(data.frame(results))
    }
    x <- calc_interpolations(out, seq(1:20))
    
    # # Create an empty plot
    # plot(a[,1], xlim = c(1, nrow(results)), ylim = range(results), xlab = "Years", ylab = "Removals (tCO2eq/ha)", main = "VCS Payment Schemes", type = "l")
    #
    # # Loop through the columns and plot each as a line
    # for (col_name in colnames(results)) {
    #     lines(results[[col_name]], col = rainbow(length(colnames(results)) - 1)[which(colnames(results)[-1] == col_name)])
    # }
    # # Add a legend
    # legend("topright", legend = colnames(results), col = c("black",rainbow(length(colnames(results)) - 1)), lty = 1)
    #
    
    
    # year_size <- 12
    # years <- ceiling(nrow(out$soilC_baseline) / year_size)
    # year_mean <- NULL
    # year_mean_s <- NULL
    # year_mean_b <- NULL
    # year_mean_last_month <- NULL
    # year_mean_12m_avg <- NULL
    #
    #     for (i in 1:years) {
    #       start <- (i - 1) * year_size + 1
    #       end <- min(nrow(out$soilC_baseline), i * year_size)
    #       year_mean_s[i] <- out$soilC_scenario[end, ] |> sum()
    #       year_mean_b[i] <- out$soilC_baseline[end, ] |> sum()
    #       if (i == 1) {
    #         year_mean_last_month[i] <- year_mean_s[i] - year_mean_b[i]
    #       } else {
    #         year_mean_last_month[i] <- year_mean_s[i] - year_mean_s[i - 1]
    #       }
    #     }
    #
    #
    #     for (i in 1:years) {
    #       start <- (i - 1) * year_size + 1
    #       end <- min(nrow(out$soilC_baseline), i * year_size)
    #       year_mean_s[i] <- out$soilC_scenario[start:end, ] |> rowSums() |> mean()
    #       year_mean_b[i] <- out$soilC_baseline[start:end, ] |> rowSums() |> mean()
    #       if (i == 1) {
    #         year_mean_12m_avg[i] <- year_mean_s[i] - year_mean_b[i]
    #       } else {
    #         year_mean_12m_avg[i] <- year_mean_s[i] - year_mean_s[i - 1]
    #       }
    #     }
    
    # out <- data.frame(
    #      "all_years_last_month" = year_mean_last_month * conversion,
    #      "all_years_12m_avg" = year_mean_12m_avg * conversion,
    #      "year_intpl" = year_intpl * conversion,
    #      "n_years" = 1:years
    #      )
    x <- cbind("year" = 1:(nrow(out$soilC_scenario)/12), x * conversion)
    return(x)
  }
