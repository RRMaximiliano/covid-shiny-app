
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

df         <- read_csv("data/observatorio_nicaragua_dep.csv")
df_country <- read_csv("data/observatorio_nicaragua.csv")
df_minsa   <- read_csv("data/minsa.csv")
mapdata    <- read_rds("data/mapdata.rds")
vaccines   <- read_csv("data/vaccines.csv")

# vaccines   <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv") %>% 
# 	filter(location %in% c("Nicaragua", "Costa Rica", "El Salvador", "Honduras", "Guatemala", "Belize", "Panama"))
# write_csv(vaccines, "data/vaccines.csv")

# Theme update ------------------------------------------------------------

theme_set(theme_ipsum_rc(base_size = 14))

theme_update(
	axis.title.x = element_text(hjust = 0.5, size = 14),
	axis.title.y = element_text(hjust = 0.5, size = 14),
)


# vaccines %>%
# 	filter(!is.na(people_vaccinated)) %>%
# 	ggplot(aes(x = date, y = people_vaccinated, group = location, color = location)) +
# 	geom_line(size = 1.2, alpha = 0.8) +
# 	geom_point(alpha = 0.6) +
# 	scale_y_continuous(labels = comma) +
# 	scale_color_viridis_d(option = "turbo") +
# 	scale_x_date(breaks = date_breaks("1 months"), labels = date_format("%m/%Y")) +
# 	labs(
# 		y = "# de vacunas administradas",
# 		x = "",
# 		color = ""
# 	)

