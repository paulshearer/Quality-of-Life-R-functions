#create a Data Frame from a csv definition
#Paul Shearer 10-29-2003
#
#
#sampleDF <- create_df_from_def("<path>//df_example_def.csv")
#see df_def_example.csv in this repo for example of the def file 
#
# Note that functionality for Allowed_Values has not been implemented 
#


create_df_from_def <- function(dfDef){
  myDFdef <- read.csv(dfDef, header = TRUE,fileEncoding = "UTF-8-BOM")
  myDFdef <- str_c(myDFdef$column_name,"=",myDFdef$data_type,",")
  myDFdef <- paste(myDFdef, collapse = '')
  myDFdef <- str_sub(myDFdef,1,nchar(myDFdef)-1)
  myDFdef <- paste0("data.frame(",myDFdef,")")
  myDF <- eval(parse(text=paste0(myDFdef)))
  return(myDF)
}
