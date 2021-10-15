
library(tidyverse)
library(hrbrthemes)
library(scales)
library(ggtext)
library(glue)
library(lubridate)
library(here)

minsa <- read_csv(here("data", "final","minsa.csv"))
fecha <- max(minsa$date)

minsa %>% 
	mutate(
		cases2 = cases - lag(cases), 
		deaths2 = deaths - lag(deaths),
		cases2 = ifelse(date == "2020-03-19", cases, cases2),
		deaths2 = ifelse(date == "2020-03-27", deaths, deaths2)
	) %>% 
	filter(deaths2 != 0) %>%
	ggplot(aes(x = date, y = deaths2)) + 
	geom_col() +
	scale_x_date(breaks = date_breaks("2 months"), labels = date_format("%m-%Y")) + 
	labs(
		x = "", 
		y = "Casos diarios",
		title = "Muertes diarias por COVID-19 en Nicaragua según datos del Ministerio de la Salud",
		caption = glue("Data: MINSA | Plot: @rrmaximiliano\nÚltima actualización: {fecha}")
	) + 
	theme_ipsum_rc() +
	theme(
		legend.position = "none",
		plot.title = element_markdown(size = 24, color = "grey40", lineheight = 1.2),
		plot.subtitle = element_markdown(size = 16, color = "grey40", lineheight = 1.2),
		plot.caption = element_text(size = 12), 
		strip.text = element_text(face = "bold")
	)
	

ggsave(here::here("plots", "deaths_minsa.png"),
		 dpi = 320, height = 10, width = 20, scale = 0.7, bg = "white")

# Cases -------------------------------------------------------------------

fecha <- max(minsa$date)

months <- tibble(yday = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
								 label = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dec"))

minsa %>% 
	mutate(
		year = lubridate::year(date),
		yday = lubridate::yday(date),
		year = as_factor(year)
	) %>% 
	group_by(year) %>% 
	mutate(
		cases2 = cases - lag(cases),
		cases2 = ifelse(date == "2020-03-19", cases, cases2),
		cases2 = ifelse(date == "2021-01-01", 0, cases2)
	) %>%
	ungroup() %>% 
	filter(cases2 != 0) %>% 
	ggplot(
		aes(x = yday, y = cases2, color = year, fill = year)
	) +
	# geom_point() + 
	# geom_col() +
	geom_col(width = 4, position = position_dodge2(padding = 0.5, preserve = "single")) +
	scale_x_continuous(breaks = months$yday, labels = months$label) +
	scale_y_continuous(limits = c(0,600)) + 
	scale_color_manual(values = c("#0072B2", "#D55E00")) + 
	scale_fill_manual(values = c("#0072B2", "#D55E00")) + 
	# facet_wrap(~ year)
	labs(
		x = "", 
		y = "Casos confirmados",
		title = "Casos diarios confirmados de COVID-19 en Nicaragua en el año <span style = 'color:#0072B2;'>2020</span> y <span style = 'color:#D55E00;'>2021</span>",
		caption = glue("Datos: Ministerio de Salud de Nicaragua | Plot: @rrmaximiliano\nÚltima actualización: {fecha}")
	) + 
	theme_ipsum_rc() +
	theme(
		legend.position = "",
		plot.title = element_markdown(size = rel(1.35)),
		plot.caption = element_text(size = rel(0.95))
	)

ggsave(here::here("scrape", "plots", "casos_minsa.png"),
			 dpi = 320, height = 10, width = 20, scale = 0.7, bg = "white")
