
# Packages ----------------------------------------------------------------

library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(here)
library(zoo)

# Run source files --------------------------------------------------------

# scrape
source("01-covid_scrape.R")
source("02-covid_scrape_dep.R")
source("03-covid_scrape_dep_deaths.R")
source("07-workers_scrape.R")

# figures
source("04-acum_mes.R")
source("05-accum_week.R") 
source("06-covid_heatmap.R")
source("08-vaccines.R")
source("08-vaccines_totals.R")
source("09-deaths-minsa.R")
