# Package names
packages <- c("data.table", "DiagrammeR", "dplyr", "DT", "flexdashboard", "forestplot", "formattable", 
              "gsheet", "htmltools", "lessR", "magrittr", "meta", "metafor", "plotly", "PRISMAstatement", 
              "psych", "reactable", "readxl", "shiny", "stringr", "tidyr", "tidyverse", "wesanderson")

# This code chunk simply makes sure that all the libraries used here are installed, it will not be shown in the report (notice echo = FALSE).
if ( length(missing_pkgs <- setdiff(packages, rownames(installed.packages()))) > 0) {
  message("Installing missing package(s): ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs)
}

# Load packages
invisible(lapply(packages, library, character.only = TRUE))