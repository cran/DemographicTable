
devtools::load_all('~/Dropbox/Packages/packageAdvanced')


file.copy(from = file.path('~/Dropbox/Packages/tzh/R', paste0(c(
  'class1List'
), '.R')), to = './R', overwrite = TRUE)


removeLocalPackage('DemographicTable')
updateDESCRIPTION('.')
checkDocument('.')
checkRelease('.')




if (FALSE) {
  createPackage(
    name = 'DemographicTable', path = '~/Dropbox/Packages', code_files = c(
      'DemographicTable_info', 'DemographicTable', 'summaryText', 'class1List', 'pval_shapiro'
    ),
    Title = 'Creating Demographic Table',
    Description = 'Functions for creating demographic table with simple summary statistics, with optional comparison(s) over one or more groups.
    Numeric variables are summarized in means, standard deviations, medians, inter-quartile-ranges (IQR), 
    skewness, Shapiro-Wilk normality test and ranges, and compared using two-sample t-test, 
    Wilcoxon test, ANOVA and/or Kruskal-Wallis test.
    Logical and factor variables are summarized in counts and percentages and
    compared using chi-squared test and/or Fisher\'s exact test.',
    Imports = c('flextable', 'e1071', 'xtable'),
    Suggests = c('officer', 'MASS'),
    to_release = FALSE
  )
}
