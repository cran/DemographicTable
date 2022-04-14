

#' @title Summary Text
#' 
#' @description Provide the summary text of an R object
#' 
#' @param x an R object
#' 
#' @param fmt see \code{\link[base]{sprintf}}
#' 
#' @param ... potential parameters
#' 
#' @return 
#' 
#' \code{\link{summaryText}} returns a \code{\link[base]{character}} scalar
#' 
#' @examples 
#' x = rpois(n = 20L, lambda = 2)
#' x[sample.int(length(x), 3L)] = NA_integer_
#' summaryText(x)
#' 
#' # factor 
#' x = state.region
#' x[2L] = NA_integer_
#' summaryText(x)
#' 
#' # binary
#' summaryText(c(TRUE, FALSE, TRUE, NA))
#' summaryText(c(TRUE, FALSE, TRUE))
#' summaryText(c(FALSE, FALSE, NA))
#' summaryText(c(FALSE, FALSE, FALSE))
#' summaryText(c(NA, NA, NA))
#' 
#' 
#' @export
summaryText <- function(x, fmt, ...) UseMethod('summaryText')

#' @export
summaryText.default <- function(x, fmt = '%.2f', ...) {
  
  # 'numeric', 'integer', 'difftime', etc
  
  if (!(typeof(x) %in% c('double', 'integer'))) stop('input must have type double or integer')
  x_orig <- unclass(x)
  x <- x_orig[is.finite(x_orig)] # NA/Inf/-Inf removed
  
  #nm <- c('\n\tmean\u00B1sd\n\tmedian\u00B1IQR\n\tgeometric mean\n\tSkewness; Shapiro-Wilk\n\tRange')
  nm <- c('\n\tmean\u00B1sd\n\tmedian\u00B1IQR\n\tSkewness; Shapiro-Wilk\n\tRange')
  
  if (!(n <- length(x))) return(setNames('', nm = nm))
  
  qs <- quantile(x, probs = c(.25, .5, .75))
  
  out <- paste(
    
    if (n < length(x_orig)) paste0('N*=', n) else '', # align with 'varname:' in DemographicTable
    
    .locscale(location = mean.default(x), scale = sqrt(var(x, na.rm = FALSE)), scale_default = ' (sd=0)', fmt = fmt), # mean +/- sd
    
    # .locscale(median.default(x), mad(x), fmt = fmt), # median +/- MAD; obsolete
    
    # paste(sprintf(fmt = fmt, qs), collapse = '\u275a'), # quantiles; obsolete
    
    .locscale(location = qs[2L], scale = qs[3L]-qs[1L], scale_default = ' (IQR=0)', fmt = fmt), # median +/- IQR
    
    #  if (any(x <= 0)) '' else { # geometric mean (geometric sd too difficult for the clinicians)
    #    logx <- log(x)
    #    # EnvStats::geoSD(c(1, 1, 1, 1)) # = 1; I don't think it make sense..
    #    #geosd <- if (is.na(.geosd <- sd(logx))) NA_real_ else if (.geosd == 0) 0 else exp(.geosd)
    #    sprintf(exp(mean.default(logx)), fmt = fmt)
    #  },
    
    if (n == 1L) '\t' else {
      x_skew <- skewness(x)
      p_shapiro <- tryCatch(shapiro.test(x)$p.value, error = identity)
      if (is.na(x_skew)) '\t' else {
        if (inherits(p_shapiro, what = 'error')) {
          sprintf(fmt = '%.1f', x_skew) 
        } else if (p_shapiro < .05) {
          sprintf(fmt = '%.1f \u26A0', x_skew)
          #sprintf(fmt = '%.1f; \u26A0 %.3f', x_skew, p_shapiro)
        } else sprintf(fmt = '%.1f \u2713', x_skew) #sprintf(fmt = '%.1f; %.3f', x_skew, p_shapiro)
      }
    },
    
    if (n == 1L) '\t' else {
      minx <- min(x)
      maxx <- max(x)
      if (minx == maxx) '\t' else paste(sprintf(fmt = fmt, c(minx, maxx)), collapse = '~') # range
    },
    
    sep = '\n')

  names(out) <- nm
  return(out)
}



.locscale <- function(location, scale, scale_default = ' (no dispersion)', fmt = '%.2f', ...) {
  if (!is.numeric(location) || !(nl <- length(location))) stop('location must be numeric')
  if (!is.numeric(scale) || !(ns <- length(scale))) stop('scale must be numeric')
  if (anyNA(location)) stop('lacation cannot contain NA')
  loc <- sprintf(fmt = fmt, location)
  scl <- rep(scale_default, times = ns)
  if (any(id <- (!is.na(scale) & (scale != 0)))) scl[id] <- sprintf(fmt = paste0('\u00B1', fmt), scale[id])
  # stats::sd returns NA_real_ for single obs
  # stats::mad returns 0 for single obs
  return(paste0(loc, scl))
}



#' @export
summaryText.factor <- function(x, fmt = '%.1f', useNA = 'no', ...) {
  if (useNA == 'ifany') stop('useNA either \'no\' or \'always\' to guarantee equal names for all \'groups\' in DemographicTable')
  if (!length(x)) return('')
  ct <- table(c(x), useNA = useNA, ...) 
  # not using ?simpletable.factor, for easy sub-packaging
  # ?base::c important, there might be 'matrix'-'factor' for ?DemographicTable
  out0 <- ifelse(ct == 0L, yes = '-', no = sprintf(fmt = paste0('%d (', fmt, '%%)'), ct, 1e2*ct/sum(ct)))
  nm <- paste(c('', names(ct)), collapse = '\n\t')
  if (!any(xok <- !is.na(x))) return(setNames('', nm = nm))
  out <- c(if (!all(xok)) paste0('N*=', sum(xok)) else '', out0)
  return(setNames(paste(out, collapse = '\n'), nm = nm))
}

#' @export
summaryText.ordered <- summaryText.factor

#' @export 
summaryText.character <- function(x, ...) summaryText.factor(factor(x), ...)




#' @export
summaryText.logical <- function(x, fmt = '%.1f', ...) {
  xok <- !is.na(x)
  if (!any(xok)) return('')
  ct1 <- sum(x[xok])
  out0 <- if (ct1 == 0L) '-' else sprintf(fmt = paste0('%d (', fmt, '%%)'), ct1, 1e2*ct1/sum(xok))
  paste(c(if (!all(xok)) paste0('N*=', sum(xok)), out0), collapse = '\n')
}


