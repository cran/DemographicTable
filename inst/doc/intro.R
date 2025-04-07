## -----------------------------------------------------------------------------
#| message: false
library(DemographicTable)
library(flextable)
set_flextable_defaults(font.size = 9)


## -----------------------------------------------------------------------------
#| echo: false
library(knitr) # for tables in this vignette
#options(mc.cores = 1L) # for CRAN submission


## -----------------------------------------------------------------------------
tgr = ToothGrowth |> 
  within.data.frame(expr = {
    dose = factor(dose) 
  })


## -----------------------------------------------------------------------------
tgr |>
  DemographicTable(include = c('supp', 'len', 'dose')) |> 
  as_flextable()


## -----------------------------------------------------------------------------
tgr |>
  DemographicTable(groups = 'supp', include = c('len', 'dose')) |> 
  as_flextable()


## -----------------------------------------------------------------------------
tgr |>
  DemographicTable(groups = 'supp', include = c('len', 'dose'), compare = FALSE) |> 
  as_flextable()


## -----------------------------------------------------------------------------
tgr |>
  DemographicTable(groups = c('supp', 'dose'), include = c('len', 'supp')) |>
  as_flextable()


## -----------------------------------------------------------------------------
tb1 = CO2 |>
  DemographicTable(groups = 'Type', include = c('conc', 'uptake'))


## -----------------------------------------------------------------------------
tb2 = CO2 |>
  subset(subset = (Treatment == 'nonchilled')) |> 
  DemographicTable(groups = 'Type', include = c('conc', 'uptake'), data.name = 'CO2_nonchilled')


## -----------------------------------------------------------------------------
c(tb1, tb2) |> as_flextable()


## -----------------------------------------------------------------------------
MASS::survey |>
  DemographicTable(groups = c('M.I'), include = c('Pulse', 'Fold')) |>
  as_flextable()


## -----------------------------------------------------------------------------
mtc = mtcars |>
  within.data.frame(expr = {
    vs_straight = as.logical(vs)
    am_manual = as.logical(am)
  })


## -----------------------------------------------------------------------------
tryCatch(DemographicTable(mtc, groups = 'am_manual', include = c('drat', 'vs_straight')), warning = identity)


## -----------------------------------------------------------------------------
mtc |>
  DemographicTable(groups = 'am_manual', include = c('drat', 'vs_straight')) |>
  as_flextable() |>
  suppressWarnings()


## -----------------------------------------------------------------------------
mtcars |>
  within.data.frame(expr = {
    vs = ifelse(vs, yes = 'Straight', no = 'V-shaped')
    am = ifelse(am, yes = 'manual', no = 'automatic')
  }) |> 
  DemographicTable(groups = 'am', include = c('drat', 'vs'), data.name = 'mtcars') |>
  as_flextable()

