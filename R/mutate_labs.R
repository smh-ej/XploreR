# SPSS Label mutate wrapper functions -----------------------------------------------
# Eugene Joh, 2018
# these functions create a new column with the labels for the selected columns (typically from SPSS or Stata import)

# general mutate wrapper functions
# by explicit selection of column

#' Mutate Wrapper for Foreign Labels
#' 
#' This \code{mutate_lab_foreign} wrapper function uses the \code{\link{mutate}} 
#' function from the \code{dplyr} to create a new column containing the foreign
#' labels from a selected column.
#'
#' @param df a \code{data frame} object to identify missing values
#' @param column_name the name of the specified column (uses standard evalulation typical of \code{dplyr})
#' @param regex a \code{character} string containing a regular expression to match a column or columns 
#'
#' @return Returns a \code{data frame} with a newly appended column
#' contain the respective foreign labels from the \code{haven} package for each row
#' based on the specified column.  
#' 
#' @import purrr tidyr dplyr haven
#' 
#' @examples
#' mutate_lab_foreign(airquality, Solar.R) #throws error because no columns contain foreign labels
#' 
#' @export
#' 
mutate_lab_foreign <- function(df, col_name = NULL, regex = NULL) {
  
  if (is.null(regex)) {
    varname <- paste0(deparse(substitute(col_name)),"_lab")
    cols <- enquo(col_name)
    
    out <- mutate(df, !!varname := haven::as_factor(!!cols))
  }

  if (!is.null(regex)) {
    if (sum(grepl(regex,names(df))) == 0) {
      stop("no matches with regular expression")
    }
    
    if (sum(grepl(regex,names(df))) == 1) {
      varname <- paste0(grep(regex,names(df),value=TRUE),".lab")
      
      out <- mutate_at(df, .vars = vars(matches(regex)),
                       .funs = funs(!!varname := haven::as_factor))
      
    } else if (sum(grepl(regex,names(df))) != 1) {
      out <- mutate_at(df, .vars = vars(matches(regex)),
                       .funs = funs(lab = haven::as_factor))
      
      names(out) <- gsub("_lab",".lab",names(out))
    }
    
  }
  
  return(out)
}


# by regex matching (wrapper for mutate_at)
mutate_at_labs <- function(df, regex) {
  require(haven)
  
  if (sum(grepl(regex,names(df))) == 0) {
    stop("no matches with regular expression")
  }
  
  if (sum(grepl(regex,names(df))) == 1) {
    varname <- paste0(grep(regex,names(df),value=TRUE),".lab")
    
    out <- mutate_at(df, .vars = vars(matches(regex)),
                     .funs = funs(!!varname := haven::as_factor))
    
  } else if (sum(grepl(regex,names(df))) != 1) {
    out <- mutate_at(df, .vars = vars(matches(regex)),
                     .funs = funs(lab = haven::as_factor))
    
    names(out) <- gsub("_lab",".lab",names(out))
  }
  
  return(out)
}



