# Extracting SPSS Variable Labels from Data Frame
# Eugene Joh, 2018

# Description:
# user defined function where input is a tidy data frame imported via the haven() package originally SPSS file
# output returns named vector of labels used for the column
# the column must be explicitly defined in function parameters

# vars = names(df), input can be any string that contains at least one column name from `df`

spss_col_labels_vec <- function(df, col_name = NULL) {
  if(is.null(col_name)) {
    stop('col_name must be explicitly defined for function')
  }
  
  require(haven)
  
  attributes(df[[col_name]])$label
  
}


spss_code_labels_vec <- function(df, col_name = NULL) {
  if(is.null(col_name)) {
    stop('col_name must be explicitly defined for function')
  }
  
  require(haven)
  
  attributes(df[[col_name]])$labels

}


