# Identifying Missing Values in Data Frame
# Eugene Joh, 2018

# Description:
# user defined function where input is a tidy data frame
# output a data frame of summarizing missing values
# output based on selected columns (default set to all columns)

# vars = names(df), input can be any string that contains at least one column name from `df`

#' Quantifies missing values found within a data frame
#' 
#' This \code{map} wrapper function vectorizes the count and proportion of missing and non-missing values within
#' a tidy data frame.
#'
#' @param df a \code{data frame} object to identify missing values
#' @param vars a \code{character string} or \code{character vector} of column names in the data frame to examine
#'
#' @return Returns a \code{data frame} with four columns, each row displays the number and percentage of missing
#' values, and the percentage of existing (non-missing) values for each variable within the data frame
#' 
#' @import purrr, tidyr, dplyr
#' 
#' @export
#'
#' @examples
#' df_missing(airquality)
#' 
df_missing <- function(df, vars = names(df)) { #identifies missing values in columns of df
  
  df[,vars] %>%
    map_dfc(~sum(is.na(.))) %>% 
    gather(key = col_name, value = missing) %>% 
    mutate(NA_percent = round(100*missing/nrow(df),2),
           exist_percent = 100-NA_percent)
  }


#' Identifies class for each variable in data frame
#' 
#' This \code{map} wrapper function vectorizes the variable data type values within a tidy data frame. 
#' Maps the \code{\link{class}} function for all select variables of a data frame.
#'
#' @param df a \code{data frame} object to identify the column class from select variables of a data frame
#' @param vars a \code{character string} or \code{character vector} of column names in the data frame to examine
#'
#' @return Returns a \code{data frame} with two columns where each row is a variable from the data frame of interest
#' and the data type ()
#' 
#' @import purrr, tidyr, dplyr
#' 
#' @export
#'
#' @examples
#' df_type(airquality)
#' 
df_type <- function(df, vars = names(df)) { #identifies the class of columns in df

  df[,vars] %>% 
    map_dfc(~class(.)) %>%
    gather(key = col_name, value = data_type)
  }


#' Identifies foreign labels within a data frame
#' 
#' This function identifies labels associated with foreign data types such as Stata or SPSS. 
#' Typically object types are imported using the \code{\link{haven}} package.
#'
#' @param df a \code{data frame} object to identify the foreign labels from select variables of a data frame
#' @param vars a \code{character string} or \code{character vector} of column names in the data frame to examine
#'
#' @return returns a \code{data frame} with a \code{logical}  column containing whether the specified 
#' columns have a foreign label associated it.
#' 
#' @import purrr, tidyr, dplyr, haven
#' 
#' @export
#' 
#'
#' @examples
#' df_type(airquality)
#' df_lab_foreign(airquality)

df_lab_foreign <- function(df, vars = names(df)) { #identifies foreign labels (imported) in columns of df
  require(purrr)
  require(tidyr)
  
  df[,vars] %>% 
    map_dfc(~is.labelled(.)) %>% 
    gather(key = col_name, value = label)
  }


df_describe <- function(df, vars = names(df),...) { #provides summary of above functions
  require(purrr)
  require(dplyr)
  
  # miss <- df_missing(df,...)
  # type <- df_type(df,...)
  # f_lab <- df_lab_foreign(df,...)
  
  if(nrow(df_missing(df,vars)) != nrow(df_type(df,vars)) | #logical check for different column sizes
     nrow(df_type(df,vars)) != nrow(df_lab_foreign(df,vars)) |
     nrow(df_missing(df,vars)) != nrow(df_lab_foreign(df,vars))) {
    stop("Number of original columns do not match, specific 'vars' in function parameters")
    
    } else {
    df1 <- df_missing(df,vars) %>% left_join(df_type(df,vars), by = "col_name") %>% 
      left_join(df_lab_foreign(df,vars), by = "col_name") %>% 
      select(col_name,everything())
    
    }
  
  return(df1)
  
  }

###############################################################################
###############################################################################
describe_viz <- function(df,title = NULL,subtitle = NULL, ...) { #visualizes the summary of above functions
  require(ggplot2)
  require(forcats)
  
  df1 <- df_describe(df,...) 
  order_name <- df1 %>% pull(col_name) #ordering names of columns (preserve original order)
  
  df1 <- df1 %>% mutate(col_name_f=factor(col_name,levels = rev(order_name)))

  df1 %>%
    ggplot(aes(x=col_name_f,y=exist_percent,fill=data_type,shape=label)) +
    geom_bar(stat='identity', colour = "grey25") + coord_flip() +
    geom_hline(yintercept = 10, colour = "red", linetype = "dashed") + #10% threshold
    labs(title = title, subtitle = subtitle,
         x = "Column Name", y = "Percentage Non-Missing (%)",...) +
    scale_fill_ochre(palette = 1,...)
  
}

# example code ####
# describe_viz(sl_raw, vars = grep("^D[1-9]",names(sl_raw),value = TRUE),
#              title = "2014 Sierra Leone Census Data")
