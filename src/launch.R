# launch.R

## Lauch Shiny app and explore questionnaire data
## but first ensure that dependencies are in place
if(!require(sensitisation, quietly = TRUE)) {
  if (!requireNamespace("devtools")) {
    cat("Installing 'devtools' from CRAN\n")
    install.packages("devtools", repos = "https://cran.rstudio.com")
  }
  cat("Installing 'sensitisation' from GitHub\n")
  devtools::install_github("NESREA/sensitisation")
}

sensitisation::display_data("data/database.csv")
