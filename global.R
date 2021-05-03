
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



# Load data ---------------------------------------------------------------

df         <- read_csv("data/observatorio_nicaragua_dep.csv")
df_country <- read_csv("data/observatorio_nicaragua.csv")
df_minsa   <- read_csv("data/minsa.csv")
mapdata    <- read_rds("data/mapdata.rds")


# Theme update ------------------------------------------------------------

theme_set(theme_ipsum_rc(base_size = 14))

theme_update(
	axis.title.x = element_text(hjust = 0.5, size = 14),
	axis.title.y = element_text(hjust = 0.5, size = 14),
)