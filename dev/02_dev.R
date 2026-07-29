# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
## install.package('attachment') # if needed.
attachment::att_amend_desc()

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "01_home", with_test = FALSE) # Name of the module
golem::add_module(name = "02_uploadData", with_test = FALSE) # Name of the module
golem::add_module(name = "03_preprocess", with_test = FALSE) # Name of the module
golem::add_module(name = "04_viewResult", with_test = FALSE) # Name of the module
golem::add_module(name = "05_downloadReport", with_test = FALSE) # Name of the module
golem::add_module(name = "06_repCheck", with_test = FALSE) # Name of the module
golem::add_module(name = "07_randomizer", with_test = FALSE) # Name of the module
golem::add_module(name = "08_lcTool", with_test = FALSE) # Name of the module
golem::add_module(name = "09_msTool", with_test = FALSE) # Name of the module
golem::add_module(name = "11_idTool", with_test = FALSE) # Name of the module
golem::add_module(name = "10_contact", with_test = FALSE) # Name of the module

## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct("cleanNames", with_test = FALSE)
golem::add_fct("formatData", with_test = FALSE)
golem::add_fct("getMeta", with_test = FALSE)
golem::add_fct("removecolumn", with_test = FALSE)
golem::add_fct("transformData", with_test = FALSE)
golem::add_fct("fillNA", with_test = FALSE)
golem::add_fct("kNNMissing", with_test = FALSE)
golem::add_fct("getCV", with_test = FALSE)
golem::add_fct("doNormalization", with_test = FALSE)
golem::add_fct("scaleData", with_test = FALSE)
golem::add_fct("showPCA", with_test = FALSE)
golem::add_fct("getFC", with_test = FALSE)
golem::add_fct("getP", with_test = FALSE)
golem::add_fct("getVIP", with_test = FALSE)
golem::add_fct("getOPLSDA", with_test = FALSE)
golem::add_fct("showSplot", with_test = FALSE)
golem::add_fct("showVolcano", with_test = FALSE)
golem::add_fct("showOPLSDA", with_test = FALSE)
golem::add_fct("prepareKMData", with_test = FALSE)
golem::add_fct("showKM", with_test = FALSE)
golem::add_fct("showBoxplot", with_test = FALSE)
golem::add_fct("showCAplot", with_test = FALSE)
golem::add_fct("showTIC", with_test = FALSE)
golem::add_fct("data", with_test = FALSE)
golem::add_fct("checkReplicates", with_test = FALSE)
golem::add_fct("getMZ", with_test = FALSE)
golem::add_fct("randomizeSeq", with_test = FALSE)
golem::add_fct("searchDB", with_test = FALSE)
golem::add_utils("xxx", with_test = FALSE)

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("custom")
golem::add_sass_file("custom")

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "cd", open = FALSE)

## Tests ----
## Add one line by test you want to create
usethis::use_test("app")

# Documentation

## Vignette ----
usethis::use_vignette()
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action()
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release()
usethis::use_github_action_check_standard()
usethis::use_github_action_check_full()
# Add action for PR
usethis::use_github_action_pr_commands()

# Travis CI
usethis::use_travis()
usethis::use_travis_badge()

# AppVeyor
usethis::use_appveyor()
usethis::use_appveyor_badge()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
