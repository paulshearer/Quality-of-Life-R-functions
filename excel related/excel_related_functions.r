
check_utf8 <- function(df) {
  
  # Detect and remove non-UTF8 characters that might cause issues in Excel
  # This function goes through every column of a dataframe and removes characters
  # that are not encoded in UTF-8. This is particularly useful when the data 
  # needs to be exported to Excel, as non-UTF8 characters might cause rendering 
  # or compatibility issues.
  # 
  # @param df: The dataframe that needs its columns checked and cleaned.
  # @return: The dataframe with non-UTF8 characters removed.
  #
  # Example: If df$a contains the string "café" with a non-UTF8 encoded "é", 
  #          check_utf8(df) would modify df$a to "caf".
  #
  # Paul Shearer
  # 10/29/2023
  
  # Loop over each column of the dataframe
  for (col in colnames(df)) {
    
    # Check if the column is of factor data type
    if (inherits(df[[col]], "factor")) {
      # Convert non-UTF8 characters to an empty string in factor levels
      levels(df[[col]]) <- iconv(levels(df[[col]]), "UTF-8", "UTF-8", sub = "")
      
      # Check if the column is of character data type
    } else if (inherits(df[[col]], "character")) {
      # Convert non-UTF8 characters to an empty string in character vectors
      df[[col]] <- iconv(df[[col]], "UTF-8", "UTF-8", sub = "")
    }
  }
  
  # Return the cleaned dataframe
  return(df)
}

uppercaseColumnNames <- function(df) {
  # Convert column names to uppercase
  names(df) <- toupper(names(df))
  return(df)
}

write_worksheet <- function(workbook, worksheet_name, data_frame_func) {
  
  # Writes data to a specified worksheet within an Excel workbook.
  # This function is intended to beautify the Excel output by adjusting column widths, 
  # applying styles, formatting date columns, and other aesthetic changes. 
  # 
  # @param workbook: The target Excel workbook.
  # @param worksheet_name: The name of the worksheet where the data should be written.
  # @param data_frame_func: The dataframe containing the data to be written.
  # 
  # @return: The modified workbook with the new worksheet.
  # 
  # Key Features:
  # 1. Column headers are bold and centered.
  # 2. Columns with names containing "date" are formatted with a "yyyy-mm-dd" style.
  # 3. Underscores in column names are replaced with spaces.
  # 4. Columns are auto-sized based on content width with a maximum width of 30 units.
  # 5. NAs in character columns are replaced with "--".
  # 6. Data is checked for UTF-8 compatibility.
  # 7. The first row (headers) is frozen for easier scrolling in Excel.
  # 
  # Paul Shearer
  # 10/29/2023
  
  # Create styles for headers, center alignment, and date format
  header_st <- createStyle(textDecoration = "Bold", halign = "center", wrapText = TRUE)
  center_style <- createStyle(halign = "center", wrapText = TRUE)
  date_style <- createStyle(numFmt = "yyyy-mm-dd", halign = "center")
  
  # Add a new worksheet to the workbook
  addWorksheet(workbook, sheetName = worksheet_name)
  
  # Replace underscores in column names with spaces for better readability
  colnames(data_frame_func) <- gsub("_", " ", colnames(data_frame_func))
  
  # Convert columns with "date" in their name to Date class
  date_cols <- grep("(?i)date", colnames(data_frame_func))
  data_frame_func[, date_cols] <- lapply(data_frame_func[, date_cols], as.Date)
  
  # Replace NAs with '--' for character columns
  data_frame_func <- data_frame_func %>%
    mutate_if(is.character, ~replace_na(., '--'))
  
  # Write the cleaned data to the specified worksheet
  writeData(workbook,
            sheet = worksheet_name,
            check_utf8(data_frame_func),
            rowNames = FALSE,
            colNames = TRUE,
            startRow = 1,
            headerStyle = header_st,
            borders = "none",
            borderColour = "black")
  
  # Calculate optimal column widths based on content, with a max width of 30 units
  col_widths <- lapply(check_utf8(data_frame_func), function(x) {
    if(length(x) > 0 && !all(is.na(x))) {
      return(max(nchar(x), na.rm = TRUE))
    } else {
      return(0)
    }
  })
  
  # Apply the calculated widths, ensuring a minimum width of 10 units
  col_widths <- sapply(col_widths, function(width) {
    width <- min(30, width + 2)
    if (is.na(width) || width < 10) width <- 10
    width
  })
  
  # Adjust the worksheet's column widths accordingly
  setColWidths(workbook, sheet = worksheet_name, cols = 1:ncol(data_frame_func), widths = col_widths)
  
  # Styles for word wrapping and centering content
  word_wrap_style <- createStyle(wrapText = TRUE)
  rows_to_wrap <- 2:(nrow(data_frame_func) + 1)
  
  # Apply the styles to the data rows
  addStyle(workbook, sheet = worksheet_name, style = word_wrap_style, rows = rows_to_wrap, cols = 1:ncol(data_frame_func), gridExpand = TRUE)
  addStyle(workbook, sheet = worksheet_name, style = center_style, rows = rows_to_wrap, cols = 1:ncol(data_frame_func), gridExpand = TRUE)
  
  # Apply date format style to date columns
  if(length(date_cols) > 0) {
    rows_to_format <- 2:(nrow(data_frame_func) + 1)
    addStyle(workbook, sheet = worksheet_name, style = date_style, rows = rows_to_format, cols = date_cols, gridExpand = TRUE)
  }
  
  # Freeze the first row for user convenience in Excel
  freezePane(workbook, sheet = worksheet_name, firstRow = TRUE)
  
  # Return the updated workbook
  return(workbook)
}





