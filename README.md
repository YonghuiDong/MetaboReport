# MetaboReport

Shiny app for metabolomics data preprocessing, statistical analysis, visualisation, and self-contained HTML reporting.

## News

Release of v0.3.6
- User manual updated
- Logo updated
- Faster in data analysis
- Bug fixed in report generation
- Report content generated
- Allow to generate Chinese report

## Installation

You can use one of the following ways to use and install MetaboReport.

#### (a) Install as a standalone App

>**Note**
> This is the easiest way to install and use MetaboReport (recommended)

<img align="center" height="50" src="https://edent.github.io/SuperTinyIcons/images/svg/windows.svg"> [Download](https://sourceforge.net/projects/metaboreport/)

#### (b) Inatall as an R package
```r
remotes::install_github("YonghuiDong/MetReport")
```
>**Note**
> After installation, you can use `library(MetaboReport); run_app()` or `MetaboReport::run_app()` to start the app.

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

## Citation
Dong, Y. and Malitsky, S., 2024. MetaboReport: from metabolomics data analysis to comprehensive reporting. Bioinformatics, 40(6), p.btae373. DOI: 10.1093/bioinformatics/btae373
