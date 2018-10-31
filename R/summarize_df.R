# Summary Statistics using dplyr
# Eugene Joh, 2018

# Description:
# user defined function where input is a tidy data frame and output a data frame of summary statistics
# summary statistics based on selected columns (default set to all columns)
# function may break if columns are not continuous (type numeric, dbl, integer)

#' Summary Statistics for Continuous Variables
#' 
#' This function returns summary statistics 
#' (mean, median, minimum, maximum, standard deviation, counts, and missing values) for
#' continuous variables found in a datamfrae
#' 
#' @param df a \code{data frame} object to report summary statistic values
#' @param vars is a \code{character} string (single length or not) of the variables of interest. 
#' The default value is it selects all column in \code{df}. 
#'
#' @return Returns a \code{data frame} where each row is a column from the original
#' data frame and each column is a summary statistic. 
#' 
#' @section Warning:
#' If columns with non-continuous values (numeric or integer) exist in \code{df}, an error 
#' is thrown and returns potential columns that you will have to remove
#' for the function to work. 
#' 
#' @import purrr tidyr dplyr
#' 
#' @examples
#' summarise_df(airquality)
#' 
#' @export
#' 
summarise_df <- function(df, vars = names(df)) {
  require(tidyr)
  require(dplyr)
  
  if (!all(sapply(df[,vars], is.numeric))) 
    stop(paste0("Non-continuous variables exists? '", names(df)[!sapply(df, is.numeric)],"'"))
  
  out <- df %>% 
    summarise_at(.vars = vars,
                 funs(min = min(.,na.rm=TRUE),
                      median = median(.),
                      max = max(.,na.rm=TRUE),
                      mean = mean(.,na.rm=TRUE),
                      sd = sd(.,na.rm=TRUE),
                      n.exist = sum(!is.na(.)))) %>% 
    gather(stat, val) %>% 
    extract(.,stat,into=c("var","stat"),regex = "(.*)_(\\S+)") %>% 
    spread(stat, val)
  
  return(out)
}

#' Counts and Proportions for Categorical Variables
#' 
#' This function returns descriptives for categorical variables 
#' (reports total counts (n) and percentage of total)
#' 
#' @param df a \code{data frame} object to report descriptives on
#' @param ... names of categorical variables to report the descriptives by group 
#'
#' @return Returns a \code{data frame} with total counts by group and percentage of total by group
#' 
#' @section Warning:
#' If columns with non-continuous values (numeric or integer) exist in \code{df}, an error 
#' is thrown and returns potential columns that you will have to remove
#' for the function to work. 
#' 
#' @import purrr tidyr dplyr
#' 
#'
#' @examples
#'iris %>% 
#'  mutate(Bin = sample(x = 1:3,size = 150,replace = TRUE)) %>% 
#'  count_cat_df(Species, Bin)
#' 
#' @export
#' 
count_cat_df <- function(df, ...) {
  require(tidyr)
  require(dplyr)

    df %>% group_by(...) %>% 
    summarise(n=n()) %>% 
    mutate(percent=100*n/sum(n))
  
}

