# Creating Factor Levels From for Age Variable
# Eugene Joh, 2018

# Description:
# user defined function where input is a tidy data frame containing a column with age (as.numeric)
# output a data frame of summarizing missing values
# parameters can choose methods based on categorization
#    used in the 2015 Sierra Leone Census Mortality Tables Excel spreadsheet
# output based on methods outlined below, this is a mutating operation/funcvtion

# Methods
# 1: <1, to 75+ by 5yrs
# 2: 0-4, ... 70-74, 75+
# 3: 0,1,2,3,...,119,120+
# 4: <1, 1-4, 5-9, ..., 95-99, 100+
# 5: 0-4, 5-9, ..., 75-79, 80+


age_categorize <- function(df,col_name = NULL, method = NULL, var_name = NULL) {
  if (is.null(col_name)) {
    stop('required to explicitly define name of column with age information')
  }
  if (is.null(method)) {
    stop('required to explicitly define method of age categorization')
  }
  
  if (is.null(var_name)) {
    v_name <- "age_cat"
  } else {
    v_name <- paste0(var_name)
  }
  
  require(dplyr)
  
  if(method == 3) {
    # Method 3: 0,1,2,3,...,119,120+
    df$age_cat <- factor(ifelse(df[[col_name]] >= 120,"120+",df[[col_name]]))
    
    return(df)
    
  } else {
    
    if(method == 1) {
      # Method 1: <1, to 75+ by 5yrs
      breaks <- c(-Inf,0,seq(from=4,to=74,by=5),Inf)
      labels <- c("<1","1-4","5-9","10-14","15-19", "20-24","25-29","30-34","35-39","40-44",
                  "45-49","50-54","55-59","60-64","65-69","70-74","75+")

    } else if(method == 2) {
      # Method 2: 0-4, ... 70-74, 75+
      breaks <- c(-Inf,seq(from=4,to=74,by=5),Inf)
      labels <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44",
                  "44-49","50-54","55-59","60-64","65-69","70-74","75+")
      
    } else if(method == 4) {
      # Method 4: <1, 1-4, 5-9, ..., 95-99, 100+
      breaks <- c(-Inf,0,seq(from=4,to=99,by=5),Inf)
      labels <- c("<1","1-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
                  "40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79",
                  "80-84","85-89","90-94","95-99","100+")
      
    } else if(method == 5) {
      # Method 5: 0-4, 5-9, 10-14,... 80+ UN demogratic standard
      breaks <- c(-Inf,4,seq(9,79,by=5),Inf)
      labels <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44",
                  "45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+")
      
    }
    
    
    
    df %>% mutate(
      age_cat0 := as.numeric(.[[col_name]]), #coerce to numeric type
      !!v_name := cut(age_cat0,breaks=breaks,labels=labels) #apply breaks and labels
      ) 

  }
  
}

# Example Check:
# age_categorize(deaths2015,"D4",1) %>% select(age_cat) %>% table()
