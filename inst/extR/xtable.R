

# not updated!!!
# 'DemographicTable' was still 'matrix'


#' @title Write [DemographicTable] to LaTeX
#' 
#' @description Write [DemographicTable] to LaTeX.
#' 
#' @param x a [DemographicTable] object
#' 
#' @param ... additional parameters of \link[xtable]{xtable}
#' 
#' @returns 
#' 
#' Function [xtable.DemographicTable] returns an \link[xtable]{xtable} object.
#' 
#' @examples 
#' (tb = DemographicTable(ToothGrowth, groups = 'supp'))
#' library(xtable)
#' print(xtable(tb), sanitize.text.function = identity, 
#'  sanitize.colnames.function = NULL, include.rownames = FALSE)
#' 
#' @importFrom xtable xtable
#' @export xtable.DemographicTable
#' @export
xtable.DemographicTable <- function(x, ...) {
  row_break <- function(x) {
    # `x` is row-1 matrix
    x0 <- c(rownames(x), x)
    x0 <- gsub('\u00B1', replacement = '\\\\pm ', x = x0)
    x0 <- gsub('\u2713', replacement = '\\\\checkmark ', x = x0)
    x0 <- gsub('\u26A0', replacement = '\\\\times ', x = x0)
    y0 <- strsplit(x0, split = '\n')
    ny <- lengths(y0, use.names = FALSE)
    n <- max(ny)
    y1 <- lapply(y0, FUN = \(i) c(i, rep('', times = n - length(i))))
    do.call(cbind, args = y1)
  }
  
  y0 <- do.call(rbind, args = lapply(seq_len(dim(x)[1L]), FUN = \(i) {
    row_break(x[i, , drop = FALSE])
  }))
  cnm <- gsub(pattern = '\\n', replacement = ' ', dimnames(unclass(x))[[2L]])
  colnames(y0) <- c(attr(x, which = 'data.name', exact = TRUE), cnm)
  
  y1 <- as.data.frame.matrix(y0, make.names = FALSE, row.names = FALSE)
  return(xtable(y1, ...))
}


