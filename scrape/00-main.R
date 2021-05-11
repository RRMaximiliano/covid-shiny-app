
# Packages ----------------------------------------------------------------

library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(here)
library(zoo)


# Run source files --------------------------------------------------------

source("01-covid_scrape.R")
source("02-covid_scrape_dep.R")
source("03-covid_scrape_dep_deaths.R")
source("07-workers_scrape.R")

# End 