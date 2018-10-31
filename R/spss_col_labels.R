#' Extracts Column Labels from a Foreign Data Frame
#' 
#' This function extracts any foreign column labels from a \code{data frame} input. It 
#' searches the \code{attributes} of the \code{data frame} input for named values  as \code{$label}.  
#' 
#' @param df a \code{data frame} object to identify missing values
#'
#' @return Returns a \code{data frame} with each column from the \code{data frame} input as a row and 
#' the corresponding foreign label (SPSS, Stata, SAS) for any applicable columns. 
#' Any columns with no foreign labels return \code{NA} values for the \code{column_labels} column.  
#' 
#' @import purrr tidyr dplyr haven
#' 
#'
#' @examples
#' spss_col_labels(airquality)
#' 
#' @export
#' 
spss_col_labels <- function(df) {
  
  spss_labels <- vector(mode="logical",length=ncol(df)) #create empty vector
  
  for (i in seq_along(df)) { #for all column names
    if (is.null(attributes(df[[i]])$label)) { #check if label is NULL
      attributes(df[[i]])$label <- NA #coerce all labels that are NULL to NA
    } 
    spss_labels[[i]] <- attributes(df[[i]])$label #input all SPSS column name labels into empty vec
  }
  #return a data frame that has...
  data.frame(columns=names(df), #original column names 
             column_labels=spss_labels, # associated SPSS labels
             stringsAsFactors = FALSE) #remove all factor data types

}

#' Extract Code Labels for a Foreign Labelled Column
#' 
#' This function extracts any foreign code labels of a specified column from a \code{data frame} input. 
#' This is analagous to the \code{\link{levels}} function that extracts the labels or levels
#' from a \code{factor} data type. 
#' 
#' @param df a \code{data frame} object to identify missing values
#' @param vars a column name (uses standard evalauation) in the data frame to examine any foreign labels 
#'
#' @return a \code{data frame} containing each code or label associated with each unique value (or level) 
#' found in the specific column. 
#' If there are no foreign labels, then a zero-dimnensional \code{tibble} is returned
#' 
#' @import purrr tidyr dplyr haven
#' 
#'
#' @examples
#' spss_code_labels(airquality, Month)
#' 
#' @export
#' 
spss_code_labels <- function(df, vars = NULL) {
  cols <- enquo(vars)
  
  as_tibble(as.list(attributes(df %>% pull(!!cols))$labels)) %>% 
    gather(key=value,value=!!cols)
}

#' Extracts Column Labels from a Foreign Data Frame
#' 
#' This function extracts any foreign column labels from a \code{data frame} input. It 
#' searches the \code{attributes} of the \code{data frame} input for named values  as \code{$label}.  
#' 
#' @param df a \code{data frame} object to identify missing values
#'
#' @return Returns a \code{characer} vector with each column from the \code{data frame} input as a row and 
#' the corresponding foreign label (SPSS, Stata, SAS) for any applicable columns. 
#' Any columns with no foreign labels return \code{NA} values for the \code{column_labels} column.  
#' 
#' @import purrr tidyr dplyr haven
#' 
#'
#' @examples
#' spss_col_labels(airquality)
#' 
#' @export
#' 
spss_col_labels_vec <- function(df, col_name = NULL) {
  if(is.null(col_name)) {
    stop('col_name must be explicitly defined for function')
  }
  
  require(haven)
  
  attributes(df[[col_name]])$label
  
}

#' Extract Code Labels for a Foreign Labelled Column
#' 
#' This function extracts any foreign code labels of a specified column from a \code{data frame} input. 
#' This is analagous to the \code{\link{levels}} function that extracts the labels or levels
#' from a \code{factor} data type. 
#' 
#' @param df a \code{data frame} object to identify missing values
#' @param vars a column name (uses standard evalauation) in the data frame to examine any foreign labels 
#'
#' @return a named \code{character} vector containing each code or label associated with each unique value (or level) 
#' found in the specific column. 
#' If there are no foreign labels, then a zero-dimnensional \code{tibble} is returned
#' 
#' @import purrr tidyr dplyr haven
#' 
#'
#' @examples
#' spss_code_labels(airquality, Month)
#' 
#' @export
#' 
spss_code_labels_vec <- function(df, col_name = NULL) {
  if(is.null(col_name)) {
    stop('col_name must be explicitly defined for function')
  }
  
  require(haven)
  
  attributes(df[[col_name]])$labels
  
}
