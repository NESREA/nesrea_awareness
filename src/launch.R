# launch.R

## Switch from packrat to User library
if (any(grepl('packrat', .libPaths()))) {
  packrat::off(print.banner = FALSE)
  Sys.sleep(3)
}

# Dependencies
if (!require(sensitisation)) {
  if (!requireNamespace("devtools"))
    install.packages('devtools', repos = "https://cran.rstudio.com")
  devtools::install_github("NESREA/sensitisation")
}

## Lauch Shiny app and explore questionnaire data
sensitisation::display_data("data/cleaned-data.rds")
