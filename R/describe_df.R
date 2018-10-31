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
#' @import purrr tidyr dplyr
#'
#'
#' @examples
#' missing_df(airquality)
#'
#' @export
#'
missing_df <- function(df, vars = names(df)) { #identifies missing values in columns of df

  df[, vars] %>%
    map_dfc(~sum(is.na(.))) %>%
    gather(key = col_name, value = n_missing) %>%
    mutate(percent_missing = round(100 * n_missing / nrow(df), 2),
           percent_exist = 100 - percent_missing)
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
#' @import purrr tidyr dplyr
#'
#' @export
#'
#' @examples
#' type_df(iris)
#'
type_df <- function(df, vars = names(df)) { #identifies the class of columns in df

  df[, vars] %>%
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
#' @import purrr tidyr dplyr haven
#'
#' @export
#'
#'
#' @examples
#' type_df(airquality)
#' lab_foreign_df(airquality)
lab_foreign_df <- function(df, vars = names(df)) { #identifies foreign labels (imported) in columns of df
  require(purrr)
  require(tidyr)

  df[,vars] %>%
    map_dfc(~is.labelled(.)) %>%
    gather(key = col_name, value = label)
  }

#' Summarizes Missing Data, Data Types, and Foreign Labels into a tidy data frame
#'
#' This function uses \code{type_df}, \code{missing_df}, \code{lab_foreign_df} to provide information on the
#' variables contained within the \code{df} input.
#'
#' @param df a \code{data frame} object to identify the foreign labels from select variables of a data frame
#' @param vars a \code{character string} or \code{character vector} of column names in the data frame to examine
#'
#' @return returns a \code{data frame} where column from \code{df} is a row and
#' information on the number of missing values, data type, and foreign labels are specified.
#'
#' @import purrr tidyr dplyr haven
#'
#' @export
#'
#' @examples
#' describe_df(airquality)
#'
describe_df <- function(df, vars = names(df), ...) {
  require(purrr)
  require(dplyr)

  # miss <- missing_df(df,...)
  # type <- type_df(df,...)
  # f_lab <- lab_foreign_df(df,...)

  if(nrow(missing_df(df, vars)) != nrow(type_df(df, vars)) | #logical check for different column sizes
     nrow(type_df(df, vars)) != nrow(lab_foreign_df(df, vars)) |
     nrow(missing_df(df, vars)) != nrow(lab_foreign_df(df, vars))) {
    stop("Number of original columns do not match, specific 'vars' in function parameters")

    } else {
    df1 <- missing_df(df,vars) %>% left_join(type_df(df,vars), by = "col_name") %>%
      left_join(lab_foreign_df(df, vars), by = "col_name") %>%
      select(col_name, everything())

    }

  return(df1)

}

#' Plots Missing Data, Data Types, and Foreign Labels into a tidy data frame
#'
#' This function takes the \code{describe_df} function, built on
#' \code{type_df}, \code{missing_df}, \code{lab_foreign_df} to plot information on the
#' variables contained within the \code{df} input.
#'
#' @param df a \code{data frame} object to identify the foreign labels from select variables of a data frame
#' @param vars a \code{character string} or \code{character vector} of column names in the data frame to examine
#'
#' @return returns a \code{ggplot2} plot displaying the percentage of missing values and data types
#' for each column contained in the data frame
#'
#' @import purrr tidyr dplyr haven
#'
#' @export
#'
#' @examples
#' describe_df(airquality)
#' describe_viz(airquality)
#'
describe_viz <- function(df, title = NULL,subtitle = NULL, threshold = 10, ...) { #visualizes the summary of above functions
  require(ggplot2)
  require(forcats)

  df1 <- describe_df(df, ...)
  order_name <- df1 %>% pull(col_name) #ordering names of columns (preserve original order)

  df1 <- df1 %>% mutate(col_name_f=factor(col_name,levels = rev(order_name)))

  df1 %>%
    ggplot(aes(x=col_name_f,y=percent_missing,fill=data_type,shape=label)) +
    geom_bar(stat='identity', colour = "grey25") + coord_flip() +
    geom_hline(yintercept = threshold, colour = "red", linetype = "dashed") + #10% threshold
    labs(title = title, subtitle = subtitle,
         x = "Column Name", y = "Percentage Missing (%)",...) +
    scale_fill_brewer(type = "qual", palette = 5)

}
