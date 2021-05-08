
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
	# filter(year == 2021) %>% 
	mutate(
		cases2 = cases - lag(cases)
	)

df %>% 
	group_by(week) %>% 
	summarize(
		sum = sum(cases2, na.rm = TRUE)
	) %>% 
	ggplot(aes(x = week, y = sum)) +
	geom_col(color = "black", fill = "#028482") + 
	geom_text(aes(label = sum), vjust = -0.5, family = roboto) + 
	scale_x_continuous(expand = c(0,0)) + 
	labs(
		x = "Semana", 
		y = "Número de casos sospechosos por semanas", 
		title = "Total de casos sospechosos acumulados por semanas en el año 2021 en Nicaragua",
		caption = paste0("Datos: Observatorio Ciudadano COVID-19, Nicaragua\nÚltimo día de actualización: ", format(max_date, "%B %d, %Y"))
	) +
	theme(
		plot.caption = element_text(hjust = 0, size = 12)
	)

ggsave(here::here("plots", "accum_weeks.png"),
			 dpi = 320, height = 9, width = 16, scale = 0.8)


df_comp <- df %>% 
	group_by(year, week2) %>% 
	summarize(
		sum = sum(cases2, na.rm = TRUE)
	) %>% 
	ungroup() %>%
	mutate(
		week2 = ifelse(week2 == 53 & year == 2021, NA, week2),
		week2 = ifelse(week2 == 18 & year == 2021, NA, week2)
	) %>%
	filter(!is.na(week2)) 

df_comp_max <- df_comp %>% 
	group_by(year) %>% 
	filter(sum == max(sum))

df_comp %>% 
	ggplot(aes(x = week2, y = sum, group = year, color = as_factor(year))) +
	geom_vline(xintercept = 12, linetype = "dotdash", color = "grey20") + 
	geom_line(size = 1.2) +
	geom_text(data = df_comp_max, aes(label = sum), family = roboto, vjust = -0.5, color = "black") + 
	scale_color_manual(values = c("#028482", "#983732")) + 
	scale_x_continuous(expand = c(0,0)) + 
	labs(
		x = "Semana", 
		y = "Número de casos sospechosos por semanas", 
		title = "Total de casos sospechosos por semanas en el año <span style = 'color:#028482;'>2020</span>
y <span style = 'color:#983732;'>2021</span> de COVID-19 en Nicaragua",
		subtitle = "La semana 18 del año 2021 (semana del 3 de mayo 2021) solo contiene información para los días lunes y martes; por lo tanto, se ha omitido.\nLa línea punteada indica la semana 12 del año 2020. La cual fue la primera semana con casos sospechosos reportados.", 
		caption = paste0("Datos: Observatorio Ciudadano COVID-19, Nicaragua | Plot: @rrmaximiliano \nÚltimo día de actualización: ", format(max_date, "%d-%m-%Y"))
	) +
	theme(
		plot.caption = element_text(hjust = 0, size = 12),
		plot.title = element_markdown(),
		legend.position = "none"
	)

ggsave(here::here("plots", "accum_weeks_comp.png"),
			 dpi = 320, height = 9, width = 16, scale = 0.8)
	