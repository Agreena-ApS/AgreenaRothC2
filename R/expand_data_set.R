#' @title Expand google sheets datasets
#' @description expand nested soil carbon values in sequential rows
#' @name expand_data_set
#' @param x googlesheet dataset
#' @author Marcos Alves
#' @import tidyr
#' @export


expand_data_set <-
  function(x) {
    unlist_null <- function(x) {
      if (is.null(x)) {
        return(NA)
      }
      if (is.list(x)) {
        return(sapply(x, unlist_null))
      }
      return(x)
    }

    new_rows <- data.frame()

    x$`SOC Start Converted` <- x$`SOC Start Converted` %>% unlist_null()
    x$`SOC End Converted` <- x$`SOC End Converted` %>% unlist_null()
    x$`Sampling depth increment` <- x$`Sampling depth increment` %>% unlist_null()
    col_depth <- which(colnames(x) == "Sampling depth increment")
    col_end <- which(colnames(x) == "SOC End Converted")
    col_start <- which(colnames(x) == "SOC Start Converted")

    toremove <- rep_len(0, length.out = nrow(x))
    for (i in 1:nrow(x)) {
      split_start <- strsplit(as.character(x$`SOC Start Converted`[i]), ",")[[1]]
      split_end <- strsplit(as.character(x$`SOC End Converted`[i]), ",")[[1]]
      split_depth <- as.numeric(strsplit(as.character(x$`Sampling depth increment`[i]), ",")[[1]])
      profile_depth <- split_depth

      # checking if all splots have the same inputs
      if (!(length(split_start) == length(split_end) & length(split_end) == length(split_depth))) {
        stop(paste("Sampling depth increment, SOC End Converted and `SOC End Converted don't have the same length in F.id:", x$`Field ID`[i]))
      }

      y <- data.frame()
      if (length(split_start) > 1) {
        # Calculate the soil profile depth and not the size
        for (j in 2:length(split_depth)) {
          profile_depth[j] <- split_depth[j] - split_depth[j - 1]
        }

        for (j in 1:length(split_start)) {
          y <- rbind(y, x[i, ])
        }

        for (j in 1:length(split_start)) {
          y[j, col_depth] <- as.character(profile_depth[j])
          y[j, col_end] <- as.character(split_end[j])
          y[j, col_start] <- as.character(split_start[j])
        }
        new_rows <- rbind(new_rows, y)
        toremove[i] <- 1
      }
    }

    # toremove[is.na(toremove)] <- 0
    toremove <- as.logical(toremove)
    x <- rbind(x[!toremove, ], new_rows)
    x <- x[order(x$`Field ID`), ]
    x[, col_depth] <- as.numeric(unlist(x[, col_depth]))
    x[, col_end] <- as.numeric(unlist(x[, col_end]))
    x[, col_start] <- as.numeric(unlist(x[, col_start]))

    x <- x %>% tidyr::drop_na(`SOC Start Converted`, `SOC End Converted`, `Sampling depth increment`, CFGs)

    return(x)
  }
