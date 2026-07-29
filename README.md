# MetaboReport

MetaboReport is a golem-based Shiny application for metabolomics data
preprocessing, statistical analysis, visualisation, and self-contained HTML
reporting.

## Run the app

```r
pkgload::load_all()
MetaboReport::run_app()
```

Use `max_upload_mb` to change the upload limit:

```r
MetaboReport::run_app(max_upload_mb = 1000)
```

## Project structure

- `R/app_*.R` contains application configuration, shared state, UI, and server
  composition.
- `R/mod_*.R` contains Shiny modules. Modules communicate through the state
  object created by `new_app_state()`.
- `R/fct_*.R` contains pure analysis, input/output, plotting, and report
  functions.
- `R/ui_components.R` contains reusable UI elements.
- `inst/app/www/` contains packaged web and report resources.
- `tests/testthat/` contains automated regression tests.
- `dev/` contains development and deployment helpers and is excluded from the
  built package.

The supported public package API is intentionally small: `run_app()`. Analysis
helpers are internal so their implementation can evolve without creating an
accidental compatibility contract.

## Report pipeline

The report module only collects reactive analysis results. The pure
`render_metabo_report()` service validates those results, creates an isolated
working directory, renders `inst/app/www/Report.Rmd`, and copies the generated
HTML to Shiny's exact download destination.

## Development checks

```r
roxygen2::roxygenise()
testthat::test_local()
rcmdcheck::rcmdcheck(args = c("--no-manual", "--no-build-vignettes"))
```

Restart the R session after changing module or report code; an already-running
Shiny process keeps the previously loaded functions in memory.
