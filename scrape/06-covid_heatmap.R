
# Packages ----------------------------------------------------------------

library(hrbrthemes)
library(tidyverse)
library(ggtext)
library(scales)
library(patchwork)
library(here)

# Load data ---------------------------------------------------------------

full_df <- read_rds(here("data", "final", "observatorio_nicaragua_dep.Rds"))

roboto = "Roboto Condensed"
brks = c("2020-03", "2020-05", "2020-07", "2020-09", "2020-11", "2021-01", "2021-03")

# Cases -------------------------------------------------------------------

max_date <- max(full_df$date)
format(max_date, "%b %d, %Y")

cases_df <- full_df %>% 
	group_by(departamento) %>% 
	arrange(departamento, date) %>% 
	mutate(
		departamento = as_factor(departamento),
		year_month = lubridate::floor_date(date, "month"), 
		year_month = format(year_month, format("%Y-%m"))
	) %>% 
	ungroup() %>% 
	select(year_month, departamento, cases) %>%
	group_by(year_month, departamento) %>% 
	summarize(
		cases = sum(cases),
	) %>% 
	mutate(
		cases = ifelse(cases == 0, NA, cases)
	)


# Deaths ------------------------------------------------------------------

deaths_df <- full_df %>% 
	group_by(departamento) %>% 
	mutate(
		departamento = as_factor(departamento),
		deaths2 = deaths - lag(deaths),
		deaths2 = ifelse(row_number()==1, deaths, deaths2),
		year_month = lubridate::floor_date(date, "month"), 
		year_month = format(year_month, format("%Y-%m"))
	) %>% 
	ungroup() %>% 
	select(year_month, departamento, deaths = deaths2) %>%
	group_by(year_month, departamento) %>% 
	summarize(
		deaths = sum(deaths),
	) %>% 
	mutate(
		deaths = ifelse(deaths == 0, NA, deaths)
	)


# Plots -------------------------------------------------------------------

 p1 <- cases_df %>% 
	ggplot(aes(x = as.factor(year_month), y = departamento, fill = cases)) +
	geom_tile() + 
	scale_x_discrete(breaks = brks, guide = guide_axis(check.overlap = TRUE)) + 
	scale_y_discrete(limits = rev) +
	scale_fill_viridis_c(option = "magma", direction = -1, trans = "sqrt", na.value="white", labels = comma) +
	coord_cartesian(expand = FALSE) +
	guides(fill = guide_colorbar(title.position = "top", 
															 title.hjust = .5,
															 barwidth = unit(20, "lines"), 
															 barheight = unit(1, "lines"))) +
	labs(
		fill = "Casos sospechosos",
		x = "",
		y = ""
	) + 
	theme_ipsum_rc(base_size = 14) +
	theme(
		axis.title.x = element_text(hjust = 0.5, size = 12),
		axis.title.y = element_text(hjust = 0.5, size = 12),
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(), 
		plot.title = element_markdown(),
		legend.position = "top",
		legend.title = element_text(family = roboto)
	)
		
p2 <- deaths_df %>% 
	ggplot(aes(x = as.factor(year_month), y = departamento, fill = deaths)) +
	geom_tile() + 
	scale_x_discrete(breaks = brks, guide = guide_axis(check.overlap = TRUE)) + 
	scale_y_discrete(limits = rev) +
	scale_fill_viridis_c(option = "magma", direction = -1, trans = "sqrt", na.value = "white") +
	coord_cartesian(expand = FALSE) +
	guides(fill = guide_colorbar(title.position = "top", 
															 title.hjust = .5,
															 barwidth = unit(20, "lines"), 
															 barheight = unit(1, "lines"))) +
	labs(
		fill = "Muertes sospechosas",
		x = "",
		y = ""
	) + 
	theme_ipsum_rc(base_size = 14) +
	theme(
		axis.title.x = element_text(hjust = 0.5, size = 12),
		axis.title.y = element_text(hjust = 0.5, size = 12),
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(), 
		plot.title = element_markdown(),
		legend.position = "top",
		legend.title = element_text(family = roboto)
	)


# Final plot and save -----------------------------------------------------

patchwork <- p1 + p2

patchwork + 
	plot_annotation(
		title = "Casos y muertes sospechosas de COVID-19 por mes en Nicaragua",
		caption = paste0("Datos: Observatorio Ciudadano COVID-19, Nicaragua\nÚltimo día de actualización: ", format(max_date, "%B %d, %Y"), "\nPlot: @rrmaximiliano"),
		theme = theme(plot.title = element_text(family = roboto, size = 24, hjust = 0.5, face = "bold"),
									plot.caption = element_text(family = roboto, size = 16))
	) 

ggsave(here("plots", "heat_map.png"),
			 dpi = 320, height = 10, width = 20, scale = 0.8, bg = "white")

