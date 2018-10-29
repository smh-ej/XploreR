# Joining Labels to  using dplyr
# Eugene Joh, 2018

# DEPRECATED

# Description:
# user defined function where input is a tidy data frame and column of interest
# output is a data frame containing the column labels and column names
#  which can be joined to aggregate summaries

# e.g.
# df %>% count(column_name) %>% left_join(df_label_define,by = "column_name)


df_label_define <- function(df, vars = NULL) {
  
  if(is.null(vars)) {
    stop('explicit selection of column using string is required!')
  }
  
  require(dplyr)
  require(tidyr)
  require(purrr)
  
  col_labels <- attributes(df[[vars]])$labels
  
  as_tibble(as.list(col_labels)) %>% gather(key=def)
  
}

# # example
# defs <- df_label_define(sl_raw,"PROVINCE")
# 
# prov_cnt %>% left_join(.,defs, by=c("PROVINCE"="value"))
# 
# df_label_define(sl_raw,"PROVINCE")
# 
# # direct code substitute
# prov_cnt %>% mutate(labels = names(attributes(.[[1]])$labels))
