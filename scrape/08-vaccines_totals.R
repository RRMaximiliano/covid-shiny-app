
# Packages ----------------------------------------------------------------

library(ggrepel)
library(tidyverse)
library(scales)


# Data and settings -------------------------------------------------------

jetbrains <- "JetBrains Mono"

Sys.setlocale("LC_TIME", "Spanish_Spain.1252")

vacunas <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv") %>%
	filter(location %in% c("Nicaragua", "Costa Rica", "El Salvador", "Honduras", "Guatemala", "Belize", "Panama")) 


# Cleaning ----------------------------------------------------------------

df_plot <- vacunas %>% 
	filter(location %in% c("Nicaragua", "Costa Rica", "El Salvador")) %>% 
	select(location, iso_code, date, total_vaccinations:people_fully_vaccinated) %>% 
	pivot_longer(
		cols = c(total_vaccinations:people_fully_vaccinated),
		names_to = "vars",
		values_to = "count"
	) %>% 
	filter(!is.na(count)) %>% 
	mutate(
		vars = str_replace_all(vars, "_", " "),
		vars = str_to_title(vars),
		vars = as_factor(vars),
		vars = case_when(
			vars == "Total Vaccinations" ~ "Total de vacunas administradas", 
			vars == "People Vaccinated" ~ "Personas vacunadas", 
			vars == "People Fully Vaccinated" ~ "Personas completamente vacunadas"
		)
	) 

vacunas_last <- df_plot %>% 
	group_by(location) %>% 
	filter(date == max(date)) 
	

# Plot --------------------------------------------------------------------

df_plot %>% 
	ggplot(
		aes(
			x = date, 
			y = count,
			group = location, 
			color = location
		)
	) + 
	geom_line(size = 1.2) + 
	geom_text_repel(
		data = vacunas_last,
		aes(
			label = scales::comma(count)
		), 
		direction = "x", 
		hjust = "right", 
		vjust = -0.2,
		family = jetbrains,
		show.legend = FALSE
	) +
	scale_y_continuous(labels = scales::label_number_si(), position = "right", limits = c(0, 8000000)) + 
	scale_x_date(breaks = date_breaks("months"), labels = date_format("%b")) + 
	scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) + 
	coord_cartesian(clip = "off", expand = FALSE) + 
	facet_wrap(~ vars, scales = "free_y") +
	labs(
		x = "",
		y = "",
		color = "",
		title = "Personas (completamente) vacunadas y total de vacunas administradas",
		subtitle = "<span style = 'color:#009E73;'>Nicaragua</span> presenta niveles bajos de total de vacunas administradas",
		caption = glue("Data: Our World in Data | Plot: @rrmaximiliano\nÚltima actualización: {fecha}")
	) + 
	theme_ipsum_rc() +
	theme(
		legend.position = "top", 
		legend.text = element_text(size = rel(1.2)), 
		axis.text.x = element_text(size = rel(1.2)),
		axis.text.y.right = element_text(size = rel(1.2), family = jetbrains, vjust = -0.25, margin = margin(l = 0, r = -10)),
		plot.title = element_markdown(size = rel(2), color = "grey40", lineheight = 1.2),
		plot.subtitle = element_markdown(size = rel(1.6), color = "grey40", lineheight = 1.2),
		plot.caption = element_text(size = rel(1.15)), 
		panel.grid.minor.x = element_blank(),
		panel.grid.major.x = element_blank(),
		strip.text = element_text(size = rel(1.4), face = "bold", color = "grey40")
	)

# Save --------------------------------------------------------------------

ggsave(
	here::here("plots", "vaccines_totals.png"),
	dpi = 320, height = 10, width = 20, scale = 0.7, bg = "white"
)
