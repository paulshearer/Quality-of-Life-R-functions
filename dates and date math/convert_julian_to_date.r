# Convert Julian to date
# This function converts a Julian date format into a standard date format.
# The function assumes the Julian date is provided as a string, where the first character represents the century
# (0 for 1900s, and 1 for 2000s), the next two characters represent the year, and the last four characters represent 
# the day of the year. 
#
# @param Column: The input column (in Julian format) to be converted. 
# @param Name: The name for the output.
# @return: The column converted to standard date format.
# 
# Example: Convert_Julian_to_date("019001", "Date") would return "1900-01-01"
#
# Paul Shearer
# 10/29/2023

Convert_Julian_to_date <- function(Column, Name){
  
  # Remove any commas from the input column, if present
  Column <- gsub(",", "", Column)
  
  # Convert the Julian format to standard date
  # - First, determine the century and year by reading the first three characters.
  # - Then, determine the day of the year by reading the last four characters.
  # - Use lubridate to convert these components into a date.
  Name <- lubridate::ymd(as.numeric(substr(Column,2,3)) + 
                           ifelse(substr(Column,1,1)=="0"
                                  ,1900
                                  ,2000)
                         ,truncated = 2L) + lubridate::days(as.numeric(substr(Column,4,7))-1)
  return(Name)
}
