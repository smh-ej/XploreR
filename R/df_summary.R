# Summary Statistics using dplyr
# Eugene Joh, 2018

# Description:
# user defined function where input is a tidy data frame and output a data frame of summary statistics
# summary statistics based on selected columns (default set to all columns)
# function may break if columns are not continuous (type numeric, dbl, integer)

df_summary <- function(df, vars = names(df)) {
  require(tidyr)
  require(dplyr)

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

# Description:
# this is for categorical variables (reports total counts (n) and percentage of total)
# user defined function where input is a tidy datafram and output is a data frame
# runs on a single variable (column) selection
# ... must be the a single variable name in quotations or a character vector of variable names

df_count_cat <- function(df, ...) {
  require(tidyr)
  require(dplyr)
  
  group_ <- rlang::syms(...)
  
  df %>% group_by(!!!group_) %>% 
    summarise(n=n()) %>% 
    mutate(percent=100*n/sum(n))
  
}

# # Examples using SL PHC 2015 death data
# df_count_cat(death2015,"URBRUR")
# 
# df_count_cat(death2015,c("PROVINCE","URBRUR"))
# 
# # vectorized for all columns in data frame
# lapply(names(death2015),function(x) df_count_cat(death2015,x))
# 
# 
