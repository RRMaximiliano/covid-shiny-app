
# Packages ----------------------------------------------------------------

library(lubridate)
library(hrbrthemes)
library(tidyverse)
library(ggtext)

## Local time

Sys.setlocale("LC_TIME", "Spanish_Spain.1252")

# Load data ---------------------------------------------------------------

nacional <- read_rds(here::here("data", "final", "observatorio_nicaragua.rds"))


# Theme -------------------------------------------------------------------

roboto = "Roboto Condensed"
jetbrains = "JetBrains Mono"

theme_set(theme_ipsum_rc(base_size = 14))

# Plot --------------------------------------------------------------------

max_date <- max(nacional$date)
format(max_date, "%b %d, %Y")


df <- nacional %>% 
	mutate(
		week = week(date),
		week2 = isoweek(date),
		year = year(date),
		month = month(date)
	) %>% 
	filter(year == 2021) %>%
	mutate(
		cases2 = cases - lag(cases)
	)

max_week <- max(df$week)

df %>% 
	group_by(week) %>%
	summarize(
		sum = sum(cases2, na.rm = TRUE)
	) %>% 
	filter(week != max_week) %>% 
	ggplot(aes(x = week, y = sum)) +
	geom_col(color = "black", fill = "#028482", width = 1) + 
	# geom_text(aes(label = sum), vjust = -0.5, family = roboto) + 
	scale_x_continuous(expand = c(0,0)) + 
	scale_y_continuous(labels = scales::comma) + 
	labs(
		x = "Semana", 
		y = "Número de casos sospechosos por semanas", 
		title = "Total de casos sospechosos acumulados por semanas en el año 2021 en Nicaragua",
		caption = paste0("Datos: Observatorio Ciudadano COVID-19, Nicaragua\nÚltimo día de actualización: ", format(max_date, "%B %d, %Y"), "\nPlot: @rrmaximiliano")
	) +
	theme(
		axis.title.x = element_text(hjust = 0.5, size = 12),
		axis.title.y = element_text(hjust = 0.5, size = 12),
		panel.grid.major.x = element_blank(),
		panel.grid.minor.x = element_blank(), 
		# plot.title = element_markdown(),
		legend.position = "none",
		# legend.title = element_text(family = roboto)
	)

ggsave(here::here("plots", "accum_weeks.png"),
			 dpi = 320, height = 10, width = 20, scale = 0.8, bg = "white")

df <- nacional %>%
	mutate(
		week = week(date),
		week2 = isoweek(date),
		year = year(date),
		month = month(date),
		yday = yday(date),
		cases2 = cases - lag(cases),
		cases2 = if_else(date == "2020-03-18", cases, cases2)
	)

df_comp <- df %>% 
	group_by(year, week) %>% 
	summarize(
		sum = sum(cases2, na.rm = TRUE)
	) %>% 
	ungroup() %>%
	mutate(
		# week = ifelse(week2 == 53 & year == 2021, NA, week2),
		week = ifelse(week == max_week & year == 2021, NA, week)
	) %>%
	filter(!is.na(week)) 

df_comp_max <- df_comp %>% 
	group_by(year) %>% 
	filter(sum == max(sum))


months <- tibble(
	yday = c(1, 6, 10, 14, 18, 23, 27, 31, 36, 40, 45, 49),
	label = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dec")
)


df_comp %>% 
	ggplot(aes(x = week, y = sum, group = year, color = as_factor(year))) +
	geom_vline(xintercept = 12, linetype = "dotdash", color = "grey20") + 
	geom_line(size = 1.2) +
	geom_text(data = df_comp_max, aes(label = scales::comma(sum)), family = jetbrains, vjust = -0.5, color = "black", check_overlap = TRUE) + 
	scale_color_manual(values = c("#028482", "#983732")) + 
	scale_x_continuous(
		# breaks = months$yday,
		breaks = c(1, 10, 20, 30, 40, 50),
		# labels = months$label,
		expand = c(0, 0)) +
	scale_y_continuous(expand = c(0,0), labels = comma) + 
	coord_cartesian(clip = "off") + 
	labs(
		x = "Semana", 
		y = "Número de casos sospechosos por semanas", 
		title = "Total de casos sospechosos de COVID-19 por semanas en el año <span style = 'color:#028482;'>2020</span>
y <span style = 'color:#983732;'>2021</span> en Nicaragua",
		subtitle = "La línea punteada indica la semana 12 del año 2020, la primera semana que se reportaron casos sospechosos de COVID-19 en Nicaragua.", 
		caption = paste0("Datos: Observatorio Ciudadano COVID-19, Nicaragua\nÚltimo día de actualización: ", format(max_date, "%b %d, %Y"), "\nPlot: @rrmaximiliano")
	) +
	theme_ipsum_rc() +
	theme(
		plot.title = element_markdown(size = rel(2)),
		plot.subtitle = element_markdown(size = rel(1.3), margin = margin(0,0,30,0)),
		plot.caption = element_text(size = rel(1.2), hjust = 1),
		axis.line.x = element_line(color = "black"),
		axis.line.y = element_blank(),
		axis.text.x = element_text(size = rel(1.1), vjust = -1, family = jetbrains, color = "black"),
		axis.text.y = element_text(size = rel(1.2), family = jetbrains, color = "black"),
		axis.title.x = element_text(size = rel(1.6), hjust = 1),
		axis.title.y = element_text(size = rel(1.6), hjust = 1),
		axis.ticks.x = element_line(size = .5, color = "black"), 
		axis.ticks.length.x = unit(.25, "cm"),
		strip.text = element_text(size = rel(1.5), face = "bold", hjust = 0.5),
		panel.grid.minor.x = element_blank(),
		panel.grid.major.x = element_blank(),
		legend.position = "none",
		plot.margin = margin(20, 40, 20, 40),
		axis.line = element_line(colour = "grey70")
	)

ggsave(here::here("plots", "accum_weeks_comp.png"),
			 dpi = 320, height = 10, width = 20, scale = 0.8, bg = "white")
	