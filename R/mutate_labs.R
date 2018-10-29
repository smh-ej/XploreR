# SPSS Label mutate wrapper functions -----------------------------------------------
# Eugene Joh, 2018
# these functions create a new column with the labels for the selected columns (typically from SPSS or Stata import)

# general mutate wrapper functions
# by explicit selection of column
mutate_labs <- function(df, col_name) {
  require(haven)
  
  varname <- paste0(deparse(substitute(col_name)),".lab")
  cols <- enquo(col_name)
  
  out <- mutate(df, !!varname := haven::as_factor(!!cols))
  
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


mutate_SL_ID_labs <- function(df) {
  require(haven)
  
  id_names <- c("HHID","PROVINCE","DISTRICT","CHIEFDOM","SECTION","EA","LCOUNCIL",
                "URBRUR","TYPEOFEA","LOCALITY","STRTYPE","STRUCT","HHINSTRUCT")
  
  if (isFALSE(all(id_names %in% names(df)))) {
    stop(paste0("data is missing the following ID columns",id_names[id_names %in% names(df)]))
  }
  id_names_labels <- id_names[map_lgl(df %>% select(id_names),~class(.) == "labelled")]
  
  admin_lvl_names <- c(
    grep("^DISTCOD.*",names(df),value=TRUE),
    grep("^CHIEFCOD.*",names(df),value=TRUE),
    grep("^SECTCOD.*",names(df),value=TRUE),
    grep("^EACOD.*",names(df),value=TRUE)
  )
  admin_lvl_names_labels <- admin_lvl_names[map_lgl(df %>% select(admin_lvl_names),~class(.) == "labelled")]
  
  
  out <- mutate_at(df, .vars = vars(c(id_names_labels,admin_lvl_names_labels)), 
                   .funs=funs(lab=haven::as_factor))
  names(out) <- gsub("_lab",".lab",names(out))
  
  return(out)
  
}


