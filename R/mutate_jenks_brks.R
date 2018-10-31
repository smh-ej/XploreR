#' Generate Jenks Breaks for Mapping with ggplot2
#' 
#' This function is a wrapper around the `dplyr` `mutate()` function that generates Jenks breaks using the `classIntervals` package.
#' See \code{\link{classIntervals}} for more details on methods. 
#' 
#' 
#' @section Warning:
#' The Jenks breaks do not like missing values and will give a warning message
#' when the variable of interest contains missing values!
#'
#' @param input a `data.frame` or `spatial` object
#' @param variable name (using standard evaluation) a continuous variable either `integer` or `numeric` to apply Jenks breaks
#' @param ... arguments used in `classIntervals` specific to `style = "jenks"`
#'
#' @return returns the `input` object with a new `factor` columns named as `variable_brks` that contains 
#' Jenks breaks and corresponding labels for each `factor` level
#'
#'
#' @import purrr tidyr dplyr classInt
#'
#' @examples
#' #Apply Jenks breaks on the Solar Radiation column frmo the airqualtiy dataset
#' airquality %>% mutate_jenks_brks(Solar.R)
#' 
#' \dontrun{
#' # Ebola Cause of Death joined to Chiefdom Shapefiles
#' # create Jenks breaks for deaths with 6 breaks
#' 
#' evd_shp <- evd_shp %>% mutate_jenks_brks(deaths, n = 6)
#' 
#' p_evd_deaths <- ggplot(evd_shp) + geom_sf(aes(fill=deaths_brks)) +
#' scale_fill_brewer(type = "seq", palette = 9, name = "Deaths") +
#' theme_void() 
#' }
#' @export
#' 
mutate_jenks_brks <- function(input, variable, ...) {
  var_string <- deparse(substitute(variable))
  # var_name <- enquo(variable)
  
  # add breaks
  jenks <- classIntervals(input[[var_string]], style = "jenks", ...)$brks
  
  if (any(0 == jenks)) brk_vec <- jenks
  else brk_vec <- c(0,jenks)
  
  # set labels
  lab_length <- length(brk_vec) - 1
  brk_labs <- vector(mode = "character", length = lab_length)
  
  # create labels dependent on the number of breaks specified
  for (i in seq(lab_length)) {
    
    if (any(brk_vec %% 1 != 0)) {#rounding to 2 decimal places
      brk_labs[i] <- paste0(round(brk_vec[i], 2), "-", round(brk_vec[i + 1], 2))
      
    } else if (any(brk_vec %% 1 == 0)) {
      brk_labs[i] <- paste0(brk_vec[i], "-", brk_vec[i + 1])
      
    }
  }
  
  # create labels using `cut()`
  input[[paste0(var_string, "_brks")]] <-
    cut(input[[var_string]],
        breaks = brk_vec,
        labels = brk_labs,
        include.lowest = TRUE)
  
  return(input)
  
  # labels can be changed directly by levels(input$variable) <- c("here","there","somewhere", ...)
  
}
