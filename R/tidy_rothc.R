#' Auxiliary function to transform RothC individual results in tidy long data frames.
#' @title Tidy AgreenaRothC outputs
#' @param ... list of objects output from \link{AgreenaRothC}
#' @return Long formatted soil carbon values that can be ploted by \link{plot_rothc}
#' @details Receives as inputs a list of data frames containing the output of AgreenaRothC and clean the data by keeping only the soil carbon stocks values and tranforming it from wide to long with a names for each datafram inputed.
#' @author Marcos Alves
#' @seealso \link{AgreenaRothC}, \link{plot_rothc}
#' @importFrom tidyr pivot_longer
#' @export
#'

tidy_rothc <- function(...) {
  if (nargs() > 1) {
    x <- list(...)
    z <- NULL
    for (i in 1:nargs()) {
      y <- x[[i]]["soilC_scenario"][[1]]
      y <- y[, grepl("SC", colnames(y))]
      colnames(y) <- c(paste0("SC_M_", i), paste0("SC_SD_", i))
      z <- c(z, y)
    }

    z <- data.frame(z, month = 1:nrow(y), Type = "scenario")
    z <- tidyr::pivot_longer(
      z,
      cols = grep("SC_M", colnames(z), value = TRUE),
      names_to = c("run_code_mean"),
      values_to = "mean"
    ) %>%
      tidyr::pivot_longer(
        cols = grep("SC_SD", colnames(z), value = TRUE),
        names_to = c("run_code_sd"),
        values_to = "sd"
      )

    w <- NULL
    for (i in 1:length(x)) {
      y <- x[[i]]["soilC_baseline"][[1]]
      y <- y[, grepl("SC", colnames(y))]
      colnames(y) <- c(paste0("SC_M_", i), paste0("SC_SD_", i))
      w <- c(w, y)
    }

    w <- data.frame(w, month = 1:nrow(y), Type = "baseline")
    w <- tidyr::pivot_longer(
      w,
      cols = grep("SC_M", colnames(w), value = TRUE),
      names_to = c("run_code_mean"),
      values_to = "mean"
    ) %>%
      tidyr::pivot_longer(
        cols = grep("SC_SD", colnames(w), value = TRUE),
        names_to = c("run_code_sd"),
        values_to = "sd"
      )

    res <- rbind(z, w)
    return(res)
  } else {
    x <- NULL
    assign("x", ...)

    y <- x["soilC_scenario"][[1]]
    y <- y[, grepl("SC", colnames(y))]
    colnames(y) <- c(paste0("SC_M_", 1), paste0("SC_SD_", 1))

    z <- y
    z <- data.frame(z, month = 1:nrow(y), Type = "scenario")
    z <- tidyr::pivot_longer(
      z,
      cols = grep("SC_M", colnames(z), value = TRUE),
      names_to = c("run_code_mean"),
      values_to = "mean"
    ) %>%
      tidyr::pivot_longer(
        cols = grep("SC_SD", colnames(z), value = TRUE),
        names_to = c("run_code_sd"),
        values_to = "sd"
      )

    w <- y
    w <- data.frame(w, month = 1:nrow(y), Type = "baseline")
    w <- tidyr::pivot_longer(
      w,
      cols = grep("SC_M", colnames(w), value = TRUE),
      names_to = c("run_code_mean"),
      values_to = "mean"
    ) %>%
      tidyr::pivot_longer(
        cols = grep("SC_SD", colnames(w), value = TRUE),
        names_to = c("run_code_sd"),
        values_to = "sd"
      )

    res <- rbind(z, w)
    return(res)
  }
}
