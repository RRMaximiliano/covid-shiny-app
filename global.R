
# Packages ----------------------------------------------------------------

library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(scales)
library(DT)
library(tidyverse)
library(extrafont)
library(hrbrthemes)
library(COVID19)
library(highcharter)
library(fresh)
library(viridisLite)


# Load data ---------------------------------------------------------------

df         <- read_csv("scrape/data/final/observatorio_nicaragua_dep.csv")
df_country <- read_csv("scrape/data/final/observatorio_nicaragua.csv")
df_minsa   <- read_csv("scrape/data/final/minsa.csv")
mapdata    <- read_rds("scrape/data/final/mapdata.rds")
vaccines   <- read_csv("scrape/data/final/vaccines.csv")
workers    <- read_rds("scrape/data/final/health_workers.Rds")

# COVID19::covid19("Nicaragua") %>%
#   select(id, date, confirmed, deaths) %>%
#   rename(cases = confirmed) %>%
#   filter(!is.na(cases)) %>%
#   write_csv("scrape/data/final/minsa.csv")
# 
# read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv") %>%
# 	filter(location %in% c("Nicaragua", "Costa Rica", "El Salvador", "Honduras", "Guatemala", "Belize", "Panama")) %>%
# 	write_csv("scrape/data/final/vaccines.csv")

# Theme update ------------------------------------------------------------

theme_set(theme_ipsum_rc(base_size = 14))

theme_update(
	axis.title.x = element_text(hjust = 0.5, size = 14),
	axis.title.y = element_text(hjust = 0.5, size = 14),
)

