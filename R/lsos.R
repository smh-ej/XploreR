#' List Objects in Global Environment
#'
#' This \code{lsos} function summarizes all objects found in the global environment into a \code{data frame}.
#' If using \code{RStudio}, this information can be viewed using the "Environment" Tab in the GUI.
#'
#' See this Stack Overflow post: https://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
#'
#' @examples
#' data(iris)
#' data(airquality)
#'
#' x <- letters[1:15]
#' y <- matrix(data = rnorm(n = 50),ncol = 10)
#'
#' lsos(n = 50)
#'
#' @export
#'
.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    format(utils::object.size(x), units = "auto") })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Length/Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing = decreasing), ]
  if (head)
    out <- head(out, n)
  out
}
# shorthand
lsos <- function(..., n = 10) {
  .ls.objects(..., order.by = "Size", decreasing = TRUE, head = TRUE, n = n)
}
