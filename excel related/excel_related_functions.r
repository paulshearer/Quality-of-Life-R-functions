
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

write_worksheet <- function(workbook, worksheet_name, data_frame_func, format_instructions = NULL) {
  
  
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
  # 8. format_instructions support 'currency', 'align_left' and 'highlight_light_grey' (right_align not testing)
  
  #example usage:
  
  # wb <- write_worksheet(wb
  #                       ,"name_of_worksheet"
  #                       ,myDataFrame
  #                       ,"currency=(5);align_left=(7);highlight_light_grey=(11,12)")
  #
  # In the example a new worksheet named 'name_of_worksheet' is created in the excel workbook and populated with the data from myDataFrame. 
  # Column 5 is formated as currency.  Column 7 is left aligned.  Columns 11 and 12 are highlighted as light grey.
  #
  
  
  # Create styles
  header_style <- createStyle(textDecoration = "Bold", halign = "center", wrapText = TRUE)
  center_style <- createStyle(halign = "center", wrapText = TRUE)
  left_style <- createStyle(halign = "left", wrapText = TRUE)
  right_style <- createStyle(halign = "right", wrapText = TRUE)
  currency_style <- createStyle(numFmt = "$#,##0.00", halign = "center")
  currency_left_style <- createStyle(numFmt = "$#,##0.00", halign = "left")
  currency_right_style <- createStyle(numFmt = "$#,##0.00", halign = "right")
  date_style_YYYY_MM_DD <- createStyle(numFmt = "YYYY-MM-DD", halign = "center")
  highlight_style <- createStyle(fgFill = "#D3D3D3")  # Light grey background
  
  # Add a new worksheet to the workbook
  addWorksheet(workbook, sheetName = worksheet_name)
  
  # Replace underscores in column names with spaces
  colnames(data_frame_func) <- gsub("_", " ", colnames(data_frame_func))
  
  # Replace NAs in character columns
  data_frame_func <- data_frame_func %>%
    mutate_if(is.character, ~replace_na(., '--'))
  
  # Write the data
  writeData(workbook, sheet = worksheet_name, x = data_frame_func, startRow = 1, startCol = 1, headerStyle = header_style)
  
  # Apply default center style to all columns
  addStyle(workbook, sheet = worksheet_name, style = center_style, rows = 2:(nrow(data_frame_func) + 1), cols = 1:ncol(data_frame_func), gridExpand = TRUE)
  
  # Initialize a list to keep track of styles for each column
  column_styles <- replicate(ncol(data_frame_func), list(currency = FALSE, align = "center", date = FALSE, highlight = FALSE), simplify = FALSE)
  
  # Parse format instructions and apply styles
  if (!is.null(format_instructions) && format_instructions != "") {
    instructions <- strsplit(format_instructions, ";")[[1]]
    for (instr in instructions) {
      parts <- strsplit(instr, "=")[[1]]
      style_type <- parts[1]
      columns <- as.numeric(unlist(strsplit(parts[2], "[(),]")))
      
      for (col in columns) {
        if (!is.na(col) && col > 0 && col <= ncol(data_frame_func)) {
          if (style_type == "currency") {
            column_styles[[col]]$currency <- TRUE
          }
          if (style_type == "align_left") {
            column_styles[[col]]$align <- "left"
          }
          if (style_type == "align_right") {
            column_styles[[col]]$align <- "right"
          }
          if (style_type == "date") {
            column_styles[[col]]$date <- TRUE
          }
          if (style_type == "highlight_light_grey") {
            column_styles[[col]]$highlight <- TRUE
          }
        }
      }
    }
  }
  
  # Apply the highlight style first if needed
  for (col in seq_along(column_styles)) {
    if (column_styles[[col]]$highlight) {
      addStyle(workbook, sheet = worksheet_name, style = highlight_style, 
               rows = 2:(nrow(data_frame_func) + 1), cols = col, gridExpand = TRUE, stack = TRUE)
    }
  }
  
  # Apply the other styles based on the column settings
  for (col in seq_along(column_styles)) {
    style_info <- column_styles[[col]]
    applied_style <- switch(style_info$align,
                            "left" = left_style,
                            "right" = right_style,
                            center_style) # Default to center if not left or right
    if (style_info$currency) {
      applied_style <- switch(style_info$align,
                              "left" = currency_left_style,
                              "right" = currency_right_style,
                              currency_style) # Override with currency style if needed
    }
    if (style_info$date) {
      applied_style <- date_style_YYYY_MM_DD # Override with date style if needed
    }
    # Apply the final style
    addStyle(workbook, sheet = worksheet_name, style = applied_style, 
             rows = 2:(nrow(data_frame_func) + 1), cols = col, gridExpand = TRUE, stack = TRUE)
  }
  
  # Set column widths and freeze top row
  widths <- sapply(data_frame_func, function(col) {
    if(all(is.na(col))) {
      return(10) # Default width for columns with all NAs
    } else {
      return(min(30, max(nchar(as.character(col)), na.rm = TRUE) + 2))
    }
  })
  
  setColWidths(workbook, sheet = worksheet_name, cols = 1:length(widths), widths = widths)
  freezePane(workbook, sheet = worksheet_name, firstRow = TRUE)
  
  return(workbook)
}
