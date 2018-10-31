#' Categorize Age into Groups
#' 
#' @description \code{age_categorize} conveniently generates commonly used age groups from a continuous age variable 
#' 
#' @param df data frame containing an \code{age} variable.
#' 
#' @param col_name a string character of the \code{age} variable name in \code{df}.
#' 
#' @param method is a \code{numeric} code specifying the age groupin method. See \code{Details} for more information
#' 
#' @section Details:
#' There are five pre-established age groups that can be specified using the \code{methods} parameter. 
#' Method \code{1}: 0, 1, ..., 119, 120+ (single year age groups)
#' Method \code{2}: <1, 0-4, ..., 70-74, 75+ (5 year age groups) 
#' Method \code{3}: 0-4, 5-9, ..., 70-74, 75+ (5 year age groups) 
#' Method \code{4}: <1, 1-4, ..., 95-99, 100+ (5 year age groups)
#' Method \code{5}: 0-4, 5-9, ..., 75-79, 80+ (UN demogratic standard)
#' Method \code{6}: 0-4, 5-9, 10-14, 15-59, 60+ (broad age groups)
#' Method \code{7}: 0-14, 15-59, 60+ (broad age groups)
#' 
#' The new column is generated using the \code{\link{mutate}} function from the \code{dplyr} package. 
#' 
#' @return returns a \code{data frame} with new \code{factor} column of labels for the
#' specified age-groups. 
#' 
#' @seealso \code{\link{cut}}, \code{\link{dplyr::mutate}}
#' 
#' @import dplyr
#' 
#' @examples 
#' ## Use of the \code{schizophrenia} dataset from the \code{HSAUR} package
#' \dontrun{
#' require(HSAUR)
#' age_categorize(schizophrenia, col_name = "age", method = 5)
#' }
#' 
#' @export


age_categorize <- function(df, col_name = NULL, method = NULL, var_name = NULL) {
  if (is.null(col_name)) {
    stop('required to explicitly define name of column with age information')
  }
  
  if (is.null(var_name)) {
    v_name <- "age_cat"
  } else {
    v_name <- paste0(var_name)
  }
  
  if (is.integer(df[,col_name])) df[,col_name] <- as.numeric(df[,col_name])
  
  if (!is.numeric(df[,col_name])) {
    stop('age variable must be numeric or integer type')
  }
  
  if(method == 1 | is.null(method)) {
    # Method 3: 0,1,2,3,...,119,120+
    df$age_cat <- factor(ifelse(df[[col_name]] >= 120,"120+",df[[col_name]]))
    
    return(df)
    
  } else {
    
    if (method == 2) {
      # Method 1: <1, to 75+ by 5yrs
      breaks <- c(-Inf, 0, seq(from = 4, to = 74, by = 5), Inf)
      labels <- c("<1","1-4","5-9","10-14","15-19", "20-24","25-29","30-34","35-39","40-44",
                  "45-49","50-54","55-59","60-64","65-69","70-74","75+")

    } else if (method == 3) {
      # Method 2: 0-4, ... 70-74, 75+
      breaks <- c(-Inf, seq(from = 4, to = 74, by = 5), Inf)
      labels <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44",
                  "44-49","50-54","55-59","60-64","65-69","70-74","75+")
      
    } else if (method == 4) {
      # Method 4: <1, 1-4, 5-9, ..., 95-99, 100+
      breaks <- c(-Inf, 0, seq(from = 4, to = 99, by = 5), Inf)
      labels <- c("<1","1-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
                  "40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79",
                  "80-84","85-89","90-94","95-99","100+")
      
    } else if (method == 5) {
      # Method 5: 0-4, 5-9, 10-14,... 80+ UN demogratic standard
      breaks <- c(-Inf, 4, seq(9, 79, by = 5), Inf)
      labels <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44",
                  "45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+")
      
    } else if (method == 6) {
      # Method 6: 0-4, 5-9, 10-14, 15-59, 60+
      breaks <- c(-Inf, seq(from = 4, to = 14, by = 5), 59, Inf)
      labels <- c("0-4", "5-9", "10-14", "15-59", " 60+")
      
    } else if (method == 7) {
      # Method 7: 0-14, 15-59, 60+
      breaks <- c(-Inf, 14, 59, Inf)
      labels <- c("0-14", "15-59", "60+")
    }
    
    df %>% mutate(
      age_cat0 := .[[col_name]], #coerce to numeric type
      !!v_name := cut(age_cat0,breaks=breaks,labels=labels) #apply breaks and labels
      ) 

  }
  
}
