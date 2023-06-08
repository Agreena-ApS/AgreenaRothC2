#' @export

write_log <- function() {
  save_dir <- getOption("output_version_folder")
  calling_file <- file.path(getOption("root"), "main_run.R")
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  log_file <- paste0("log_", timestamp, ".txt")
  log_path <- file.path(save_dir, log_file)
  log <- file(log_path, "w")
  session_info <- capture.output(sessionInfo())
  cat("Session Info:\n", file = log)
  cat("------------------------------------\n", file = log)
  cat(session_info, file = log, sep = "\n")
  if (!is.null(calling_file)) {
    cat("\n\nCalling File:\n", file = log)
    cat("------------------------------------\n", file = log)
    cat(readLines(calling_file), file = log, sep = "\n")
  }
  close(log)
  message("Log file created: ", log_path)
}
