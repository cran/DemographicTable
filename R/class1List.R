

#' @title class1List
#' 
#' @description List the first \code{\link[base]{class}} of each columns in a \code{\link[base]{data.frame}}
#' 
#' @param data a \code{\link[base]{data.frame}}
#' 
#' @return 
#' 
#' \code{\link{class1List}} returns a \code{\link[base]{list}} 
#' of the first \code{\link[base]{class}} of each columns in the input \code{\link[base]{data.frame}}
#' 
#' @examples 
#' class1List(esoph)
#'
#' @export 
class1List <- function(data) {
  if (!is.data.frame(data)) stop('input must be data.frame')
  data <- as.data.frame(data)
  if (anyDuplicated.default(names(data))) stop('do not allow duplicated colnames names in \'data.frame\'')
  
  cl1 <- vapply(data, FUN = function(x) class(x)[1L], FUN.VALUE = '', USE.NAMES = TRUE)
  return(split.default(names(cl1), f = factor(cl1)))
}