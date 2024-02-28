#' Check and Add Missing Columns
#'
#' This function takes a data.frame as input and checks if the specified columns are present. If any of the columns are missing, they are added to the data.frame in the specified order.
#'
#' @param df The input data.frame.
#' @return The modified data.frame with the missing columns added.
#' @examples
#' df <- data.frame(year = 2022, ini_soc = 1, Interval_1 = 0, Interval_2 = 0)
#' df_modified <- check_and_add_columns(df)
#' str(df_modified)
#' @export
check_and_add_columns <- function(df, required_columns) {
    # Identify the missing columns
    missing_columns <- setdiff(required_columns, colnames(df))

    # Add the missing columns with NA values
    df[missing_columns] <- NA

    # Get the current column names
    current_columns <- colnames(df)

    # Determine the order of the columns
    ordered_columns <- c(setdiff(current_columns, required_columns), required_columns)

    # Reorder the columns
    df <- df[, ordered_columns]

    return(df)
}
