
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
	geom_text(aes(label = sum), vjust = -0.5, family = roboto) + 
	scale_x_continuous(expand = c(0,0)) + 
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
		month = month(date)
	) %>%
	# filter(year == 2021) %>%
	mutate(
		cases2 = cases - lag(cases)
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

df_comp %>% 
	ggplot(aes(x = week, y = sum, group = year, color = as_factor(year))) +
	geom_vline(xintercept = 12, linetype = "dotdash", color = "grey20") + 
	geom_line(size = 1.2) +
	geom_text(data = df_comp_max, aes(label = sum), family = roboto, vjust = -0.5, color = "black", check_overlap = TRUE) + 
	scale_color_manual(values = c("#028482", "#983732")) + 
	scale_x_continuous(expand = c(0,0)) + 
	labs(
		x = "Semana", 
		y = "Número de casos sospechosos por semanas", 
		title = "Total de casos sospechosos por semanas en el año <span style = 'color:#028482;'>2020</span>
y <span style = 'color:#983732;'>2021</span> de COVID-19 en Nicaragua",
		subtitle = "La línea punteada indica la semana 12 del año 2020. La cual fue la primera semana con casos sospechosos reportados.", 
		caption = paste0("Datos: Observatorio Ciudadano COVID-19, Nicaragua\nÚltimo día de actualización: ", format(max_date, "%B %d, %Y"), "\nPlot: @rrmaximiliano")
	) +
	theme(
		axis.title.x = element_text(hjust = 0.5, size = 12),
		axis.title.y = element_text(hjust = 0.5, size = 12),
		panel.grid.major.x = element_blank(),
		panel.grid.minor.x = element_blank(), 
		plot.title = element_markdown(),
		legend.position = "none",
		# legend.title = element_text(family = roboto)
	)

ggsave(here::here("plots", "accum_weeks_comp.png"),
			 dpi = 320, height = 10, width = 20, scale = 0.8, bg = "white")
	