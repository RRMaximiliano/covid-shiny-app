
# Packages ----------------------------------------------------------------

library(lubridate)
library(hrbrthemes)

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

nacional %>% 
	mutate(
		week = week(date),
		year = year(date),
		month = month(date)
	) %>% 
	filter(year == 2021) %>% 
	mutate(
		cases2 = cases - lag(cases)
	) %>%
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
		caption = paste0("Notas: Observatorio Ciudadano COVID-19, Nicaragua\nÚltimo día de actualización: ", format(max_date, "%B %d, %Y"))
	) +
	theme(
		plot.caption = element_text(hjust = 0, size = 12)
	)

ggsave(here("plots", "accum_weeks.png"),
			 dpi = 320, height = 9, width = 16, scale = 0.8)

	