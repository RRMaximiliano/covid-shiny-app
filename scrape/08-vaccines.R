
# Packages ----------------------------------------------------------------

library(lubridate)
library(hrbrthemes)
library(tidyverse)
library(scales)
library(ggtext)
library(here)
library(hrbrthemes)
library(glue)

# Load df -----------------------------------------------------------------

vaccines <- read_csv(here("data", "vaccines.csv"))

Sys.setlocale("LC_TIME", "Spanish_Spain.1252")
roboto = "Roboto Condensed"

# Plots -------------------------------------------------------------------

vaccines <- vaccines %>% 
	group_by(location) %>% 
	filter(!is.na(total_vaccinations),
				 total_vaccinations != 0,
				 location != "Belize") %>% 
	mutate(
		rate = (total_vaccinations - lag(total_vaccinations)) / lag(total_vaccinations) * 100,
		location = if_else(location == "Panama", "Panamá", location)
	) %>% 
	select(date, location, total_vaccinations, rate) %>% 
	ungroup() 
	
vaccines_text <- vaccines %>% 
	group_by(location) %>% 
	filter(date == max(date))

fecha <- max(vaccines_text$date)

vaccines %>% 
	ggplot(aes(x = date, y = total_vaccinations, group = location, color = location)) +
	geom_line(size = 1.2) +
	geom_text(data = vaccines_text,
						aes(label = scales::comma(total_vaccinations)),
						check_overlap = TRUE, vjust = -0.5, family = roboto, fontface = "bold") + 
	scale_y_continuous(label = scales::comma) + 
	coord_cartesian(expand = TRUE, clip = "off") + 
	facet_wrap(~ location) + 
	labs(
		x = "",
		y = "Vacunas administradas",
		title = "<span style = 'color:#00C0B8;'>Honduras</span> y <span style = 'color:#00A5FF;'>Nicaragua</span> presentan muy bajos niveles de vacunación",
		subtitle = "En las últimas dos semanas, el total de vacunas administradas en Nicaragua apenas creció 0.6%\npasando de 166,350 a 167,500 vacunas administradas",
		caption = glue("Data: Our World in Data | Plot: @rrmaximiliano | Última actualización: {fecha}")
	) + 
	theme_ipsum_rc() +
	theme(
		legend.position = "none",
		plot.title = element_markdown(hjust = .5, size = 24, color = "grey40", lineheight = 1.2),
		plot.subtitle = element_text(hjust = .5, size = 14),
		plot.caption = element_text(size = 12), 
		strip.text = element_text(face = "bold")
	)


ggsave(here::here("scrape", "plots", "vaccines.png"),
			 dpi = 320, height = 9, width = 16, scale = 0.8)

