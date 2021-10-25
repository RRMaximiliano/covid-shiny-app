
library(lubridate)
library(hrbrthemes)
library(ggtext)
library(glue)

df <- read_rds("data/final/observatorio_nicaragua.Rds")
jetbrains <- "JetBrains Mono"

fecha <- max(df$date)

df_plot <- df %>% 
	rename(
		cases_acum = 2,
		deaths_acum = 3
	) %>% 
	mutate(
		cases  = cases_acum - lag(cases_acum),
		deaths = deaths_acum - lag(deaths_acum),
		cases  = case_when(date == "2020-03-18" ~ cases_acum, TRUE ~ cases),
		deaths = case_when(date == "2020-03-18" ~ deaths_acum, TRUE ~ deaths),
		week   = week(date),
		month  = month(date),
		year   = year(date)
	) %>% 
	group_by(year, week) %>% 
	summarise(
		across(
			c("cases", "deaths"), 
			list(sum = mean, min = min, max = max), .names = "{.col}_{.fn}")
	) 

df_plot %>% 
	ggplot(
		aes(
			x = week,
			color = as_factor(year)
		), 
	) + 
	geom_linerange(
		aes(
			ymin = cases_min, 
			ymax = cases_max
		),
		size = 5,
		alpha = 0.7,
		position = position_dodge2(preserve = "single", width = 0.35)
	) +
	geom_point(
		aes(
			y = cases_sum,
		),
		shape = "-",
		size = 10
	) + 
	coord_cartesian(expand = FALSE) + 
	scale_color_manual(values = c("#357f7a", "#bb6457")) +
	labs(
		color = "",
		y = "Weekly cases", 
		x = "",
		title = "Weekly COVID-19 cases in Nicaragua in <span style = 'color: #357f7a'>2020</span> and <span style = 'color:#bb6457;'>2021</span>",
		caption = glue("Data: Observatorio Ciudadano COVID-10 • Plot: @rrmaximiliano • Last updated: {fecha}.")
	) + 
	theme_ipsum_rc() + 
	theme(
		legend.position = "none",
		plot.title = element_markdown(color = "grey50", size = rel(2)),
		plot.caption = element_text(size = rel(1.2), hjust = 0),
		axis.title.y = element_text(size = rel(1.3)),
		axis.line.x = element_line(color = "black"),
		axis.text.y = element_text(size = rel(1.1), family = jetbrains, color = "black", vjust = -0.25),
		axis.text.x = element_text(size = rel(1.1), family = jetbrains, color = "black", vjust = -0.5),
		axis.ticks.x = element_line(size = .5, color = "black"), 
		axis.ticks.length.x = unit(.25, "cm"),
		panel.grid.minor.x = element_blank(),
		panel.grid.major.x = element_blank()
	)
	
ggsave(
	here::here("plots", "cases_linerange.png"),
	dpi = 320, height = 10, width = 20, scale = 0.8, bg = "white"
)
