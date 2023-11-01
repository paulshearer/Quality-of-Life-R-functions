#' Fix Non-Breaking Space in Dataframe
#'
#' This function replaces all non-breaking spaces (NBSP) in string (character) 
#' columns of a dataframe with regular spaces.
#'
#' @param df A dataframe with one or more character columns that might contain non-breaking spaces.
#' @return A dataframe with non-breaking spaces replaced by regular spaces in all character columns.
#' 
#' @examples
#' # Sample data frame
#' df <- data.frame(name = c("John\xc2\xa0Doe", "Jane\xc2\xa0Smith"), age = c(25, 30))
#' fixed_df <- fix_non_breaking_space(df)
#'
#' @export

fix_non_breaking_space <- function(df) {
  # Loop over all columns in dataframe
  df[] <- lapply(df, function(column) {
    # Check if column is of type character
    if (is.character(column)) {
      # Replace NBSP with regular space and return the modified column
      return(gsub("\xc2\xa0", " ", column))
    }
    # Return column as-is if it's not of type character
    return(column)
  })
  # Return the modified dataframe
  return(df)
}
