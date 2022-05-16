

#' @title Create Demographic Table
#'
#' @description
#'
#' Functions for creating demographic table with simple summary statistics, with optional comparison(s) over one or more groups.
#' Numeric variables are summarized in means, standard deviations, medians, inter-quartile-ranges (IQR), 
#' skewness, Shapiro-Wilk normality test and ranges, and compared using two-sample t-test, 
#' Wilcoxon test, ANOVA and/or Kruskal-Wallis test.
#' Logical and factor variables are summarized in counts and percentages and
#' compared using chi-squared test and/or Fisher's exact test.
#'
#' @import stats utils
#' 
#' @importFrom flextable flextable as_flextable autofit hline vline set_caption fontsize 
#' @importFrom e1071 skewness
#'
#' @docType package
#' @keywords package
#' @name DemographicTable-package
NULL
