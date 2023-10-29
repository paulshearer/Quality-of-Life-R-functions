#  Paul Sheerer
#  10/29/2023 
#
# This code runs standalone test cases against each function defined in the .\QA\testcases.csv file.

setwd('C://git//Quality-of-Life-R-functions//QA Functions//')
#rm(list=ls())

# Load dependencies
if (!require('dplyr')) install.packages('dplyr')
if (!require('stringr')) install.packages('stringr')

library(dplyr)
library(stringr)

# Smart comparison function to handle numeric and integer values without losing precision
smart_compare <- function(vec1, vec2) {
  if (is.numeric(vec1) && is.integer(vec2)) {
    vec2 <- as.numeric(vec2)
  } else if (is.integer(vec1) && is.numeric(vec2)) {
    vec1 <- as.numeric(vec1)
  }
  return(identical(vec1, vec2))
}

# Load all functions in the specified directory
files <-  list.files("C://git//Quality-of-Life-R-functions//QA Functions//functions//", full.names = TRUE, recursive = TRUE)
files <- str_replace_all(files, "(?<!/)[/](?!/)", "//")  
files <- str_replace_all(files, "///", "//")
lapply(files, source)
remove(files)

# Create DF from definition file in CSV
testCases_def <- read.csv("testCases_def.csv", header = TRUE, fileEncoding = "UTF-8-BOM")
testCases_results <- create_df_from_def("testCases_def.csv")
remove(testCases_def)

QA_testCases <- read.csv("example Test Cases for Functions.csv", header = TRUE, fileEncoding = "UTF-8-BOM")

# Loop through each row of the QA_testCases
for(i in 1:nrow(QA_testCases)){
  
  temp_actual_results <- eval(parse(text = QA_testCases$command[i]))
  
  if(QA_testCases$testCaseDescription[i] != 'Cleanup') {
    
    temp_expected_results <- eval(parse(text = QA_testCases$ExpectedResult[i]))
    
    for (col in names(temp_expected_results)) {
      result <- smart_compare(temp_expected_results[,col], temp_actual_results[,col])
      
      if(result){
        testCases_results <-
          data.frame(func_name = c(QA_testCases$func_name[i])
                     ,testCase = c(QA_testCases$testCase[i])
                     ,testCaseDescription = c(QA_testCases$testCaseDescription[i])
                     ,result = c(TRUE)
                     ,resultsDescription = paste0(col," : Matched! Expected ",temp_expected_results[,col]," got ",temp_actual_results[,col])) %>%
          bind_rows(testCases_results)
        
        print(paste0(col,i))
      } else {
        testCases_results <-
          data.frame(func_name = c(QA_testCases$func_name[i])
                     ,testCase = c(QA_testCases$testCase[i])
                     ,testCaseDescription = c(QA_testCases$testCaseDescription[i])
                     ,result = c(FALSE)
                     ,resultsDescription = paste0(col," : Matched Failed! Expected ",temp_expected_results[,col]," got ",temp_actual_results[,col])) %>%
          bind_rows(testCases_results)
      }
    }
  }
}

testCases_results <- testCases_results %>% filter(str_count(resultsDescription, "NA") < 2)

paste0(nrow(testCases_results %>% filter(result == TRUE)), " Successful")
paste0(nrow(testCases_results %>% filter(result == FALSE)), " Failed")
