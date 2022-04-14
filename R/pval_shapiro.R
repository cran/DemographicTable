


#' @title P-value from modified Shapiro-Wilk Normality Test
#' 
#' @description
#' Obtain p-value from Shapiro-Wilk normality test \code{\link[stats]{shapiro.test}}, 
#' taking into consideration of several exceptions.
#' 
#' @param x \code{\link[base]{double}} vector
#' 
#' @param CLT \code{\link[base]{logical}} scalar, whether to allow the use of Central Limit Theorem (default \code{FALSE})
#' 
#' @details 
#' 
#' \code{\link{pval_shapiro}} provides a pseudo p-value for the several exceptions of
#' \code{\link[stats]{shapiro.test}}, serving as a criteria of whether robust statistics/tests need to be used
#' \itemize{
#' \item{\code{length(x) < 3L}} {return \code{0}, robust methods needed}
#' \item{\code{length(x) > 5e3L}} {return \code{1}, no robust method needed (robust methods could be too slow)}
#' \item{\code{CLT & length(x) > 30L}} {return \code{1}, no robust method needed because of the use of Central Limit Theorem}
#' \item{all \code{x} values identical} {return \code{0}, robust methods needed.}
#' \item{Otherwise} {use the p-value from \code{\link[stats]{shapiro.test}}}
#' }
#' 
#' @return 
#' 
#' \code{\link{pval_shapiro}} returns a \code{\link[base]{double}} scalar.
#' 
#' @examples 
#' pval_shapiro(rnorm(5))
#' sapply(with(airquality, split(Ozone, f = Month)), FUN = pval_shapiro)
#' 
#' @export
pval_shapiro <- function(x, CLT = FALSE) {
  x0 <- as.double(x[!is.na(x)]) # ?stats::shapiro.test will drop NA though
  n <- length(x0)
  # R 4.0.*, ?stats::shapiro.test do not allow sample size <3L or >5e3L
  if (n < 3L) return(0) # robust methods needed for n<3L
  if (n > 5e3L) return(1) # robust methods might be too slow for n>5e3L
  if (n > 30L && CLT) return(1) # central limit theorem, no need for robust methods
  if (all(duplicated.default(x0)[-1L])) return(0) # robust methods needed for all-equal input
  out <- shapiro.test(x0) # let err (?stats::shapiro.test takes no additional parameter)
  if (is.na(pv <- unname(out$p.value))) stop('stats::shapiro.test gives NA $p.value')
  return(pv)
}





