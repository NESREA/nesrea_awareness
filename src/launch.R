# launch.R

## Switch from packrat to local library
packrat::off(print.banner = FALSE)
Sys.sleep(3)

packages <- c(sens = 'sensitisation', dev = 'devtools')
quiet <- TRUE

## Ensure that dependencies are in place
if(suppressWarnings(!require(packages['sens'], quietly = quiet, character.only = TRUE))) {
  if (try(!requireNamespace(packages['dev'], quietly = quiet))) {
    message(paste("Installing", sQuote(packages['dev']), "from CRAN... "),
            appendLF = FALSE)
    suppressWarnings(install.packages(
      pkgs = packages['dev'],
      repos = "https://cran.rstudio.com",
      quiet = quiet,
      verbose = FALSE))
    Sys.sleep(2)
    if (!(packages['dev'] %in% .packages(all.available = TRUE))) {
      message('Failed')
      stop('Could not download ', sQuote(packages['dev']))
    }
  }
  message(paste("Installing", sQuote(packages['sens']), "from GitHub... "),
          appendLF = FALSE)
  devtools::install_github(paste0("NESREA/", packages['sens']), quiet = quiet)
  Sys.sleep(1)
  rm(list = ls())
}

## Lauch Shiny app and explore questionnaire data
sensitisation::display_data("data/cleaned-data.rds")
