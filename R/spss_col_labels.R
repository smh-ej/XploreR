# Create function for extracting SPSS labels ####
# Eugene Joh, 2018

# These functions extract the SPSS labels found in a haven package import object (read_spss() or read_sav())
# output for these functions are data frames

# This function extracts the column name labels
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

# This function extracts the labels for the code values used in each column
spss_code_labels <- function(df,vars=NULL) {
  as_tibble(as.list(attributes(df[[vars]])$labels)) %>% 
    gather(key=value,value=!!vars)
}