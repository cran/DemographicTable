


# write more S3 ?
adjustColWidths.flextable <- function(x, font.size, elastic = 1.2, ...) {
  # this is rather naive as it presumes a monospaced font
  
  # ?flextable::autofit -> ?flextable::dim_pretty -> ?flextable:::optimal_sizes ->
  # -> ?flextable:::text_metric -> ?flextable:::fortify_content or ?gdtools::m_str_extents
  # does not recognize line feed '\n' (this is explicitly specified in help files ?flextable::autofit)
  # also see how ?flextable:::html_str.flextable use `$colwidths` in html output (workhorse ?flextable:::format.complex_tabpart)
  
  xdat <- x$body$dataset
  
  if (!missing(font.size)) {
    if (!is.numeric(font.size) || length(font.size) != 1L || is.na(font.size) || font.size <= 0) stop('illegal font.size')
    x <- fontsize(x, i = NULL, j = NULL, size = font.size, part = 'all')
  }
  
  if (!is.numeric(elastic) || length(elastic) != 1L || is.na(elastic) || elastic < 0) stop('illegal elastic')
  
  ratio <- 
    (colWidth(xdat, linefeed = TRUE) / colWidth(xdat, linefeed = FALSE)) * 
    (if (!missing(font.size)) font.size / 11 else 1) * # size = 11 is default of ?flextable::fontsize
    elastic
  
  bc <- length(x$body$colwidths)
  if (bc != length(ratio)) stop('flextable: body ncol different from length(xdat) ?')
  x$body$colwidths <- x$body$colwidths * ratio
  if (hc <- length(x$header$colwidths)) {
    if (bc != hc) stop('flextable: body ncol and header ncol differ?')
    x$header$colwidths <- x$header$colwidths * ratio
  }
  if (fc <- length(x$footer$colwidths)) {
    if (bc != fc) stop('flextable: body ncol and footer ncol differ?')
    x$footer$colwidths <- x$footer$colwidths * ratio
  }
  return(x)
} 






#' @title Text Width (for \pkg{flextable})
#' 
#' @description Determine the text width of screen output
#' 
#' @param x An R object convertible to \code{\link[base]{data.frame}}
#' 
#' @param linefeed see \code{\link{nchar_lf}}
#' 
#' @return 
#' 
#' \code{\link{colWidth}} returns an \code{\link[base]{integer}} vector 
#' of the print widths of a \code{\link[base]{data.frame}}
#' \strong{not} considering the row names.
#' 
#' @examples 
#' head(mtcars)
#' colWidth(mtcars)
#' 
#' @export
colWidth <- function(x, linefeed = TRUE) {
  x <- as.data.frame(x) # use S3
  n_body <- vapply(x, FUN = function(i) max(nchar_lf(i, linefeed = linefeed), na.rm = TRUE), FUN.VALUE = 0L)
  n_cnm <- nchar_lf(names(x), linefeed = linefeed)
  pmax.int(n_body, n_cnm)
}


# \code{cr} for carriage return


#' @title Number of Characters Acknowledging Line Feed
#' 
#' @description Finds the number of characters, similar to \code{\link[base]{nchar}}, 
#' while acknowledging the line feed \code{'\\n'}
#' 
#' @param x An R object convertible to \code{\link[base]{character}}
#' 
#' @param linefeed \code{\link[base]{logical}} scalar, whether to recognize the line feed in counting
#' number of characters.  Default \code{TRUE}
#' 
#' @return 
#' \code{\link{nchar_lf}} returns an \code{\link[base]{integer}} scalar or vector, 
#' the number of characters in each element of the input.
#' 
#' @examples 
#' 
#' nchar_lf(character()) # 0L
#' x = c('aa\nb', '', NA, '\n', 'cef\ncd', 'abc')
#' nchar_lf(x, linefeed = TRUE)
#' nchar_lf(x, linefeed = FALSE)
#' nchar_lf(c(TRUE, FALSE, NA))
#' nchar_lf(c(12, 3, 467))
#' 
#' @export
nchar_lf <- function(x, linefeed = TRUE) {
  x <- as.character(x) # do not use my ?lv_apply for 'factor', easier to write into sub-package
  if (!(n <- length(x))) return(0L)
  
  xok <- !is.na(x)
  id_lf <- (xok & (x == '\n'))
  id_nonlf <- (xok & !grepl(pattern = '\\n', x = x)) # grepl is slow, but worth it when large % of x is non-lf
  
  ret <- integer(length = n) # default 0L for all elements
  ret[!xok] <- 2L # most likely 'NA' won't be printed, but 2L is a safe choice
  ret[id_nonlf] <- nchar(x[id_nonlf])
  
  id <- (xok & nzchar(x) & !id_nonlf & !id_lf)
  if (!any(id)) return(ret)
  
  xsp <- strsplit(x[id], split = '\n', fixed = TRUE)
  ret[id] <- if (linefeed) {
    vapply(xsp, FUN = function(i) max(nchar(i)), FUN.VALUE = 0L)  
  } else {
    nchar(x[id]) - (lengths(xsp, use.names = FALSE) - 1L) # faster, but harder to understand :)
    # vapply(xsp, FUN = function(i) sum(nchar(i)), FUN.VALUE = 0L) # slow
  }
  return(ret)
}






