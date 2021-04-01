# Package names
packages <- c("data.table", "DiagrammeR", "dplyr", "DT", "flexdashboard", "forestplot", "formattable", 
              "gsheet", "htmltools", "Hmisc" , "lessR", "magrittr", "meta", "metafor", "plotly", "png", "PRISMAstatement", 
              "psych", "reactable", "readxl", "rsconnect", "shiny", "stringr", "summarytools", "tidyr", "tidyverse", "wesanderson", "rlang", "DescTools", "dmetar", "readr",
              "revtools" )

# This code chunk simply makes sure that all the libraries used here are installed, it will not be shown in the report (notice echo = FALSE).
if ( length(missing_pkgs <- setdiff(packages, rownames(installed.packages()))) > 0) {
  message("Installing missing package(s): ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs)
}

# Load packages
invisible(lapply(packages, library, character.only = TRUE))