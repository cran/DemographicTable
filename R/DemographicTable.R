

#' @title Create Demographic Table
#' 
#' @description Create a demographic table with simple summary statistics, with optional comparison(s) over one or more groups.
#' 
#' @param data a \code{\link[base]{data.frame}}
#' 
#' @param data.name \code{\link[base]{character}} scalar, or the argument call of \code{data}.  
#' A user-friendly name of the input \code{data}.
#' 
#' @param groups \code{\link[base]{character}} scalar or vector, 
#' the name(s) of sub-group(s) for which the summary statistics are to be provided.
#' Default \code{NULL} indicating no sub-groups.
#' 
#' @param keep_missing_group \code{\link[base]{logical}} scalar.
#' If \code{TRUE} (default), the subjects with missing \code{group}
#' are put into a new group (\code{'.missing'}).
#' if \code{FALSE}, these subjects are removed from group-wise summary statistics.
#' 
#' @param exclude \code{\link[base]{character}} vector, 
#' the name(s) of variable(s) to be excluded.  
#' Default \code{NULL} indicating no variable are to be excluded.
#' 
#' @param exclude_pattern (optional) \code{\link[base]{character}} scalar as 
#' \code{\link[base:regex]{regular expression}}, 
#' the pattern of the names of the variable(s) to be excluded. 
#' 
#' @param include \code{\link[base]{character}} vector, 
#' the name(s) of variable(s) to be included.
#' Default \code{names(data)} indicating all variables are to be included.
#' 
#' @param include_pattern \code{\link[base]{character}} scalar as 
#' \code{\link[base:regex]{regular expression}}, 
#' the pattern of the names of the variable(s) to be included.
#' 
#' @param overall \code{\link[base]{logical}} scalar.
#' If \code{TRUE} (default), a column of overall summary statistics will be provided.
#' 
#' @param compare \code{\link[base]{logical}} scalar.
#' If \code{TRUE} (default), comparisons between group(s) will be made.
#' 
#' 
#' @param ... potential parameters
#' 
#' @details 
#' 
#' A demographic table with simple summary statistics, with optional comparison(s) over one or more groups, is created.
#' 
#' \code{\link[base:numeric]{Numeric}} variables are summarized in means, standard deviations, medians, inter-quartile-ranges (IQR), 
#' skewness, Shapiro-Wilk normality test and ranges.
#' If \code{group} is specified, they are compared using two-sample \code{\link[stats:t.test]{t}}-test, 
#' \code{\link[stats:wilcox.test]{Wilcoxon / Mann-Whitney}} test, one-way \code{\link[stats:aov]{ANOVA}} and/or 
#' \code{\link[stats:kruskal.test]{Kruskal-Wallis}} test.
#' 
#' \code{\link[base]{logical}} and \code{\link[base]{factor}} variables are summarized in counts and percentages.
#' If \code{group} is specified, they are compared using \code{\link[stats:prop.test]{chi-squared}} test
#' and/or \code{\link[stats:fisher.test]{Fisher exact}} test.
#' 
#' @return 
#' 
#' \code{\link{DemographicTable}} returns an object of S3 class \code{'DemographicTable'}, 
#' which inherits from \code{\link[base]{matrix}}.
#' 
#' @examples 
#' DemographicTable(esoph)
#' DemographicTable(ToothGrowth, groups = 'supp')
#' DemographicTable(ToothGrowth, groups = 'supp', compare = FALSE)
#' DemographicTable(warpbreaks, groups = c('wool', 'tension'))
#' 
#' # write to Word file
#' library(flextable)
#' library(officer)
#' x = read_docx() |> body_add_flextable(value = as_flextable(DemographicTable(esoph)))
#' (out = file.path(tempdir(), 'demostable.docx'))
#' print(x, target = out)
#' # system(paste('open', out)) # works on Mac & Windows, but requires Microsoft Word
#' file.remove(out)
#' 
#' @export
DemographicTable <- function(
    data, data.name = substitute(data), 
    groups = NULL, keep_missing_group = TRUE,
    exclude = NULL, exclude_pattern, 
    include, include_pattern, 
    overall = TRUE, 
    compare = TRUE,
    ...
) {
  
  force(data.name)
  
  if (!is.data.frame(data)) stop('input must be data.frame')
  data <- as.data.frame(data) # use S3
  if (anyDuplicated.default(names(data))) stop('Duplicated column names in raw data?')
  data <- data[!vapply(data, FUN = function(i) all(is.na(i)), FUN.VALUE = NA)] # remove all-missing columns 
  
  if (length(groups)) {
    if (!is.character(groups) || anyNA(groups) || !all(nzchar(groups))) stop('groups must be character without NA or zchar')
    groups <- unique.default(groups)
    if (any(id <- is.na(match(groups, table = names(data), nomatch = NA_integer_)))) stop(sQuote(groups[id]), ' not in names of data. Removed accidentally?')
    if (any(id <- vapply(data[groups], FUN = is.matrix, FUN.VALUE = NA, USE.NAMES = FALSE))) stop(sQuote(groups[id]), ' is/are matrix column(s).')
  }
  
  if (!missing(exclude_pattern)) {
    exclude <- unique.default(c(exclude, grep(exclude_pattern, x = names(data), value = TRUE)))
  }
  
  include <- if (missing(include_pattern)) {
    if (missing(include)) names(data) else include
  } else {
    ptn_include <- grep(include_pattern, x = names(data), value = TRUE)
    if (missing(include)) ptn_include else unique.default(c(include, ptn_include))
  }
  
  include <- setdiff(x = include, y = c(exclude, groups)) # made sure `include` and `groups` has no overlap
  
  rm(exclude)
  
  if (any(id <- is.na(match(include, table = names(data), nomatch = NA_integer_)))) {
    message('Unknown variable(s): ', sQuote(include[id]), ' removed.')
    include <- include[!id]
  }
  
  data <- data[c(include, groups)]
  
  for (i in include) {
    if (is.character(data[[i]])) data[[i]] <- factor(data[[i]]) 
    # MUST!! otherwise missing groups in subset-data will not print zero-count
  }
  
  ##################################################################
  ## Inspect `groups` in detail (removing rows if needed)
  ##################################################################
  
  if (length(groups)) {
    
    if (keep_missing_group) {
      for (ig in groups) {
        if (any(id <- is.na(data[[ig]]))) {
          data[[ig]] <- as.character(data[[ig]])
          data[[ig]][id] <- '.missing'
        }# else do nothing
      }
    } # else do nothing!!!!
    
    for (ig in groups) {
      igv <- data[[ig]]
      if (length(unique(igv[!is.na(igv)])) == 1L) {
        message('Column ', sQuote(ig), ' has single value, thus removed from `groups`.')
        groups <- setdiff(groups, ig)
      }
    } # remove any group with all-same entries
    
  }
  
  ############################################
  ## Inspect `include` in detail
  ############################################
  
  vlst <- class1List(data[include]) # without `groups`
  
  if (length(vlst$matrix)) {
    stop('debugging for Curry-Zach study')
    mtype <- vapply(data[vlst$matrix], FUN = typeof, FUN.VALUE = '')
    if (any(id_double <- (mtype == 'double'))) {
      vlst$difftime <- c(vlst$difftime, vlst$matrix[id_double][id_difftime <- vapply(data[vlst$matrix[id_double]], FUN = inherits, what = 'difftime', FUN.VALUE = NA)])
      vlst$numeric <- c(vlst$numeric, vlst$matrix[id_double][!id_difftime])
    }
    # 'factor' 'matrix' is in `vlst$factor` already...
    if (any(id_bool <- (mtype == 'logical'))) {
      vlst$logical <- c(vlst$logical, vlst$matrix[id_bool])
    }
    vlst$matrix <- vlst$matrix[!id_bool & !id_double]
    if (length(vlst$matrix)) stop('uncovered matrix column')
  }
  
  ######################
  # Done! use `data`, `vlst` and `groups` below
  ######################
  
  ret <- if (overall) DemographicSummaries(data, vlst = vlst, ...) # else NULL      
  
  if (length(groups)) {
    ret_by <- lapply(groups, FUN = demoTab_by, data = data, vlst = vlst, compare = compare, ...)
    rets <- if (length(ret)) c(list(ret), ret_by) else ret_by
    # is_equal(rets, FUN = function(x) dimnames(x)[[1L]])
    ret <- do.call(cbind, args = rets)
    attr(ret, which = 'groups') <- groups
    attr(ret, which = 'test') <- unique.default(unlist(lapply(ret_by, FUN = attr, which = 'test', exact = TRUE), use.names = FALSE))
    attr(ret, which = 'ncols') <- vapply(rets, FUN = function(x) dim(x)[2L], FUN.VALUE = 0L)
  } else if (!overall) {
    stop('must do at least `overall` or by-`groups`')
  }

  attr(ret, which = 'data.name') <- if (is.character(data.name)) {
    if (length(data.name) != 1L || anyNA(data.name) || !nzchar(data.name)) stop('illegal data.name')
    data.name
  } else deparse1(data.name)
  class(ret) <- c('DemographicTable', class(ret))
  return(ret)
  
}





##################
## work horse
##################

DemographicSummaries <- function(data, vlst, fmt = '%.1f', ...) {
  
  
  out_num <- if (length(.num <- setNames(nm = c(vlst$integer, vlst$numeric)))) {
    unlist(lapply(data[.num], FUN = summaryText.default, fmt = fmt, ...), use.names = TRUE)
  } #else NULL
  
  out_difft <- if (length(.difft <- vlst$difftime)) {
    d_difft <- setNames(data[.difft], nm = paste0(.difft, ' (', vapply(data[.difft], FUN = attr, which = 'units', exact = TRUE, FUN.VALUE = ''), ')')) # ?base::units.difftime
    unlist(lapply(d_difft, FUN = summaryText.default, fmt = fmt, ...), use.names = TRUE)
  } #else NULL
  
  out_bool <- if (length(.bool <- vlst$logical)) {
    d_bool <- setNames(data[.bool], nm = paste0(.bool, ': n (%)'))
    vapply(d_bool, FUN = summaryText.logical, fmt = fmt, ..., FUN.VALUE = '')
  } #else NULL
  
  out_factor <- if (length(.fact <- c(vlst$character, vlst$factor, vlst$ordered))) {
    d_fact <- setNames(data[.fact], nm = paste0(.fact, ': n (%)'))
    unlist(lapply(d_fact, FUN = summaryText, fmt = fmt, useNA = 'no', ...), use.names = TRUE)
  } #else NULL

  #return(c(out_num, out_difft, out_bool, out_factor))
  ret <- c(out_num, out_difft, out_bool, out_factor)
  array(ret, dim = c(length(ret), 1L), 
        dimnames = list(names(ret), paste0('N=', .row_names_info(data, type = 2L))))
   
}




demoTab_by <- function(data, vlst, group, group_perc = TRUE, compare = TRUE, ...) { # SMD = FALSE, 
  
  if (!is.character(group) || length(group) != 1L || anyNA(group) || !nzchar(group)) stop('`group` must be len-1 character')
  
  fgrp <- factor(data[[group]])
  gidx <- split.default(seq_along(fgrp), f = fgrp)
  gN <- lengths(gidx, use.names = FALSE)
  
  ret <- do.call(cbind, args = lapply(gidx, FUN = function(id) { # (id = gidx[[1L]])
    DemographicSummaries(data[id, , drop = FALSE], vlst = vlst, ...)
  }))
  colnames(ret) <- if (group_perc) {
    sprintf(fmt = '%s\n= %s\nN=%d (%.1f%%)', group, names(gidx), gN, 1e2*gN/sum(gN))
  } else sprintf(fmt = '%s\n= %s\nN=%d', group, names(gidx), gN)
  
  # removing single 'group' for p-values
  txt_g1 <- if (any(g1 <- (gN == 1L))) {
    gidx <- gidx[-g1]
    paste0('(', sum(g1), ' ', sQuote(group), ' level(s) of\n single obs omitted)')
  } # else NULL
  
  ng <- length(gidx)
  if (ng < 2L) return(ret)
  
  if (compare) {
    .double <- vapply(c(vlst$integer, vlst$numeric, vlst$difftime), FUN = function(i) compare_double(demo_get(x = data[[i]], gidx = gidx), ...), FUN.VALUE = '')
    .bool <- vapply(vlst$logical, FUN = function(i) compare_bool(demo_get(x = data[[i]], gidx = gidx), ...), FUN.VALUE = '')
    .factor <- vapply(c(vlst$character, vlst$factor, vlst$ordered), FUN = function(i) compare_factor(x = data[[i]], g = fgrp, ...), FUN.VALUE = '')
    pval <- c(.double, .bool, .factor)
    if (dim(ret)[1L] != length(pval)) stop('demographic table contruction wrong: pval do not match summary stats')
    #p_test0 <- gsub('\\(|\\)', replacement = '', x = unique.default(str_extract(pval, pattern = '\\(.*\\)$')))
    #p_test <- p_test0[!is.na(p_test0)]
    ret_compare <- as.matrix(pval)
    colnames(ret_compare) <- paste0('Significance\n(by ', group, ')\n', txt_g1)
  } else ret_compare <- NULL #pval <- p_test <- NULL
  
#  .by2 <- (ng == 2L)
#  if (SMD && .by2) {
    
#    txt_SMD <- function(x) {
#      xci <- confint.stddiff(x)
#      ret <- sprintf(fmt = '%.3f (%.3f~%.3f)', x$coefficients, xci[,1L], xci[,2L])
#      ret[!attr(xci, which = 'ok', exact = TRUE)] <- ''
#      return(ret)
#    }
    
#    .smd_dbl <- if (length(v_dbl <- c(vlst$integer, vlst$numeric, vlst$difftime))) {
#      txt_SMD(stddiff_all(g = data[[group]], v = data[v_dbl], type = 'double'))
#    } else character()
#    
#    .smd_bool <- if (length(vlst$logical)) {
#      txt_SMD(stddiff_all(g = data[[group]], v = data[vlst$logical], type = 'logical'))
#    } else character()
#    
#    .smd_fct_order <- if (length(v_fac <- c(vlst$character, vlst$factor, vlst$ordered))) {
#      txt_SMD(stddiff_all(g = data[[group]], v = data[v_fac], type = 'factor'))
#    } else character()
    
#    SMD <- as.vector(c(.smd_dbl, .smd_bool, .smd_fct_order))
#    if (dim(ret)[1L] != length(SMD)) stop('demographic table contruction wrong: pval do not match summary stats')
#    
#  } else SMD <- NULL
  
  ret <- cbind(ret, ret_compare)
  #attr(ret, which = 'test') <- p_test
  return(ret)

}


#' @title Convert \code{\link{DemographicTable}} to \code{\link[flextable]{flextable}}
#' 
#' @description 
#' Convert a \code{\link{DemographicTable}} to \code{\link[flextable]{flextable}}.
#' 
#' @param x a \code{\link{DemographicTable}}
#' 
#' @param font.size \code{\link[base]{integer}} scalar, the font size (default 8).
#' See \code{\link[flextable]{fontsize}}
#' 
#' @param caption (optional) \code{\link[base]{character}} scalar, the table caption.
#' See \code{\link[flextable]{set_caption}}
#' 
#' @param ... potential additional parameters, not currently in use 
#' 
#' @return 
#' 
#' \code{\link{as_flextable.DemographicTable}} returns a \code{\link[flextable]{flextable}} object.
#'
#' @seealso \code{\link[flextable]{as_flextable}}
#' 
#' @export
as_flextable.DemographicTable <- function(x, font.size = 8, caption, ...) {
  x1 <- data.frame(' ' = dimnames(x)[[1L]], unclass(x), row.names = NULL, check.names = FALSE, fix.empty.names = FALSE, stringsAsFactors = FALSE)
  names(x1)[1L] <- attr(x, which = 'data.name', exact = TRUE)
  
  y0 <- adjustColWidths.flextable(autofit(flextable(data = x1)), font.size = font.size)
  y1 <- hline(y0, i = seq_len(dim(x)[1L] - 1L))
  ncols <- attr(x, which = 'ncols', exact = TRUE)
  y2 <- vline(y1, j = 1L + cumsum(ncols[-length(ncols)]))
  #ret <- vline(y2, j = 1L, border = fp_border(width = 1.5)) # do not want to import ?officer::fp_border
  ret <- vline(y2, j = 1L)
    
  if (missing(caption)) return(ret) 
  set_caption(ret, caption = caption) # parameter `caption` is handy in practice
}


# ?base::print
#' @export
print.DemographicTable <- function(x, ...) print(as_flextable.DemographicTable(x, ...))







demo_get <- function(x, gidx) {
  # `x`: 'double' responses to be compared
  # `gidx`: a 'list' of group indexes
  xm <- is.matrix(x)
  xs <- lapply(gidx, FUN = function(i) {
    y <- unclass(if (xm) c(x[i, ]) else x[i])
    y[!is.na(y)]
  })
  return(xs[lengths(xs, use.names = FALSE) > 1L])
}




symb <- function(p) { # vectorized
  ret <- character(length = length(p))
  ret[p < .05] <- '\u2605 '
  return(ret)
}

# old name `pText.pairwise.htest`
pText_pairwise.htest <- function(x) {
  dnm <- dimnames(pv0 <- x$p.value)
  id <- lower.tri(pv0, diag = TRUE)
  pv <- pv0[id]
  pnm <- outer(dnm[[1L]], dnm[[2L]], FUN = paste, sep = ' vs. ')[id]
  sprintf(fmt = paste0(symb(pv), '%.3f (%s)'), pv, pnm)
}




compare_double <- function(xs, CLT = TRUE, pairwise = 3L, alternative = c('two.sided', 'less', 'greater'), ...) {
  # @param pairwise 'integer' value, the maximum group number under which pairwise tests,
  # \code{\link[stats]{pairwise.t.test}} and \code{\link[stats]{pairwise.wilcox.test}}, are preferred.  Default value \code{3L}.
  
  ng <- length(xs)
  if (ng <= 1L) return('1 arm or less')
  alternative <- match.arg(alternative)
  
  p_shapiro <- vapply(xs, FUN = pval_shapiro, CLT = CLT, FUN.VALUE = 0, USE.NAMES = FALSE)
  
  
  
  if (ng == 2L) { # ?stats::t.test or ?stats::wilcox.test
    if (any(p_shapiro < .05)) {
      p.value <- wilcox.test(x = xs[[1L]], y = xs[[2L]], exact = FALSE, alternative = alternative)$p.value
      return(sprintf(fmt = paste0(symb(p.value), '%.3f\nWilcoxon-\nMann-Whitney'), p.value))
    }
    p.value <- t.test(x = xs[[1L]], y = xs[[2L]], alternative = alternative)$p.value
    return(sprintf(fmt = paste0(symb(p.value), '%.3f\nTwo-Sample t'), p.value))
  }

  if (!is.numeric(pairwise) || length(pairwise) != 1L || anyNA(pairwise) || pairwise < 2L) stop('illegal `pairwise`')
  # `is.numeric(pairwise)` not `is.integer(pairwise)` to allow Inf
  
  if (ng <= pairwise) {
    x <- unlist(xs, use.names = FALSE)
    g <- rep(names(xs), times = lengths(xs, use.names = FALSE))
  }
  
  if (any(p_shapiro < .05)) {
    if (ng <= pairwise) {
      tmp <- pairwise.wilcox.test(x = x, g = g, p.adjust.method = 'none', alternative = alternative)
      return(paste(c(pText_pairwise.htest(tmp), 'Wilcoxon-\nMann-Whitney'), collapse = '\n'))
    }
    return(tryCatch(expr = {
      p.value <- kruskal.test(x = x, g = g, ...)$p.value
      sprintf(fmt = paste0(symb(p.value), '%.3f\nKruskal-Wallis'), p.value)
    }, error = 'Kruskal-Wallis test\nnot available'))
  }
  
  if (ng <= pairwise) {
    tmp <- pairwise.t.test(x = x, g = g, pool.sd = FALSE, p.adjust.method = 'none', alternative = alternative)
    return(paste(c(pText_pairwise.htest(tmp), 'Two-Sample t'), collapse = '\n'))
  }
  # ?stats::aov requires formula~data parameterization
  return(tryCatch(expr = {
    p.value <- summary(aov(x ~ g))[[1L]][1L, 'Pr(>F)']
    sprintf(fmt = paste0(symb(p.value), '%.3f\nANOVA'), p.value)
  }, error = 'ANOVA not available'))
}




compare_bool <- function(xs, pairwise = 3L, alternative = c('two.sided', 'less', 'greater'), ...) {
  
  ng <- length(xs)
  if (ng <= 1L) return('1 arm or less')
  alternative <- match.arg(alternative)
  
  X <- vapply(xs, FUN = sum, FUN.VALUE = 0L, USE.NAMES = TRUE)
  N <- lengths(xs, use.names = TRUE)
  
  p.value <- fisher.test(cbind(X, N-X), alternative = alternative)$p.value
  fisher_txt <- sprintf(fmt = paste0(symb(p.value), '%.3f\nFisher\'s Exact'), p.value)
  
  if (ng == 2L) {
    if (any(X == 0L, X == N)) return('') # p-value means nothing
    return(tryCatch(expr = {
      p.value <- prop.test(x = X, n = N, alternative = alternative)$p.value
      sprintf(fmt = paste0(symb(p.value), '%.3f\nProportion test'), p.value)
    }, warning = function(w) fisher_txt))
  }
  
  if (ng <= pairwise) {
    tmp <- suppressWarnings(pairwise.prop.test(x = X, n = N, p.adjust.method = 'none', alternative = alternative))
    return(paste(c(pText_pairwise.htest(tmp), 'Proportion test'), collapse = '\n'))
  }
  
  return(fisher_txt)
  
}




compare_factor <- function(x, g, ...) {
  # will use ?stats::fisher.test or ?stats::chisq.test even if the factor has 2 levels (i.e. essentially binary)
  
  if (is.matrix(x)) g <- rep(g, times = dim(x)[2L]) # as of 2022-03-08, ?base::table will not recycle shorter argument
  tab <- table(x, g, useNA = 'no') # `x` can be either 'factor' or 'character'
  if (anyNA(tab)) stop('should not happen')
  
  tmp <- tryCatch(fisher.test(tab), error = function(e) {
    tmp <- if (grepl('simulate.p.value=TRUE', x = e$message)) {
      tryCatch(fisher.test(tab, simulate.p.value = TRUE), error = identity, warning = identity)
    } else tryCatch(chisq.test(tab), error = identity, warning = identity)
    if (inherits(tmp, what = 'error')) return('Fisher\'s exact\nnor Chi2 test available')
    if (inherits(tmp, what = 'warning')) return(suppressWarnings(chisq.test(tab)))
    return(tmp)
  })
  
  if (is.character(tmp)) return(tmp)
  p.value <- tmp$p.value
  if (grepl('^Fisher', tmp$method)) return(sprintf(fmt = paste0(symb(p.value), '%.3f\nFisher\'s Exact'), p.value))
  return(sprintf(fmt = paste0(symb(p.value), '%.3f\nChi-Dquared'), p.value))
  
}




