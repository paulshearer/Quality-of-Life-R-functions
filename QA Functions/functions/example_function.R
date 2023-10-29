# example function for the QA automation
#
example_function <- function(parameter1, parameter2, parameter3) {
  
  # Check if the parameters are numeric
  if(!is.numeric(parameter1) || !is.numeric(parameter2) || !is.numeric(parameter3)) {
    stop("All parameters should be numeric.")
  }
  
  # Ensure parameters are whole numbers (integers)
  if(parameter1 != round(parameter1) ||
     parameter2 != round(parameter2) ||
     parameter3 != round(parameter3)) {
    stop("All parameters should be whole numbers.")
  }
  
  # Check if any of the parameters are outside the 0 to 9 range
  # If any parameter is outside this range, the function stops and throws an error
  if(parameter1 < 0 || parameter1 > 9 ||
     parameter2 < 0 || parameter2 > 9 ||
     parameter3 < 0 || parameter3 > 9) {
    stop("All parameters should be between 0 and 9 inclusive.")
  }
  
  # Use the `expand.grid` function to generate all possible combinations 
  # of the values from 0 to each of the specified parameters.
  # For instance, if parameter1 is 2, then for Column1 the possible values are 0, 1, and 2.
  # Similarly, possible values for other columns are determined based on parameter2 and parameter3.
  grid <- expand.grid(Column1 = 0:parameter1, Column2 = 0:parameter2, Column3 = 0:parameter3)
  
  # Return the resulting data frame with all combinations
  return(grid)
}
