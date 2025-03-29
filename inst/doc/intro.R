## ----include = FALSE----------------------------------------------------------
library(knitr)
opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----setup--------------------------------------------------------------------
library(DemographicTable)
set_flextable_defaults(font.size = 9)

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
tb2 = CO2 |>
  subset(subset = (Treatment == 'nonchilled')) |> 
  DemographicTable(groups = 'Type', include = c('conc', 'uptake'), data.name = 'CO2_nonchilled')
c(tb1, tb2) |> as_flextable()

## -----------------------------------------------------------------------------
MASS::survey |>
  DemographicTable(groups = c('M.I'), include = c('Pulse', 'Fold')) |>
  as_flextable()

## ----echo=FALSE---------------------------------------------------------------
msg_logical() |> cat()

## -----------------------------------------------------------------------------
mtc = mtcars |>
  within.data.frame(expr = {
    vs = as.logical(vs)
    am = as.logical(am)
  })
tryCatch(DemographicTable(mtc, groups = 'am', include = c('hp', 'drat')), warning = identity)
tryCatch(DemographicTable(mtc, groups = 'cyl', include = c('vs')), warning = identity)

