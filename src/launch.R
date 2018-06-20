# launch.R

## Lauch Shiny app and explore questionnaire data

dset <- "data/database.csv"

if(!require(sensitisation, quietly = TRUE)) {
  if (!requireNamespace("devtools")) {
    cat("Installing 'devtools' from CRAN\n")
    install.packages("devtools", repos = "https://cran.rstudio.com")
  }
  cat("Installing 'sensitisation' from GitHub\n")
  devtools::install_github("NESREA/sensitisation")
}

sensitisation::display_data(dset)