library(ggplot2); library(dplyr); library(tidyr); library(readr); library(sf); library(stringr)

lib_file <- "Libraries_-_Locations___Contact_Information__and_Usual_Hours_of_Operation_20261108.csv"
ca_file  <- "Boundaries_-_Community_Areas_20261108.csv"

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
file.copy(c(lib_file, ca_file), "data/raw", overwrite = TRUE)   # raw files stay unchanged

data_sources <- tibble(
  file = c(lib_file, ca_file),
  publisher = "City of Chicago Data Portal",
  dataset = c("Libraries - Locations, Contact Information, and Usual Hours of Operation",
              "Boundaries - Community Areas"),
  url = "https://data.cityofchicago.org",
  date_accessed = "2026-11-08",
  license = "check the portal page for each dataset")
write_csv(data_sources, "data_sources.csv")