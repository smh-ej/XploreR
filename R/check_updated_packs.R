#' Convenience function for checking package versions
#'
#' This function provides a simple way to check the versions of installed
#' packages by utilizing the \code{old.packages} function from the \code{utils}
#' package.
#' 
#' See \code{\link{old.packages}}
#'
#' @importFrom utils old.packages
#'
#' @return a \code{data frame} object of all R pacakges in \code{LibPath} that 
#' have differing versions between what is installed and the CRAN Repo.
#' 
#' @export
#'
#' @examples
#' check_updated_packs()
#' 
#' 
check_updated_packs <- function(x) {
  
  old_df <- as.data.frame(old.packages())
  
  old_df[] <- lapply(old_df[], as.character)
  
  old_df[old_df$Installed != old_df$ReposVer, ]
  
}
