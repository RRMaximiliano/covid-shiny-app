library(lubridate)
library(hrbrthemes)

df_country <- read_csv("data/observatorio_nicaragua.csv")

Sys.setlocale("LC_TIME", "Spanish_Spain.1252")

# Cases
df_country %>% 
  mutate(
    cases2 = cases - lag(cases), 
    deaths2 = deaths - lag(deaths)
  ) %>% 
  mutate(
    cases2 = ifelse(date == "2020-03-18", cases, cases2),
    deaths2 = ifelse(date == "2020-03-18", deaths, deaths2)
  ) %>% 
  mutate(
    month = zoo::as.yearmon(date),
    floor = floor_date(date, "month")
  ) %>%
  group_by(floor) %>%
  summarize(
    sum_cases = sum(cases2),
    sum_deaths = sum(deaths2)
  ) %>% 
  ggplot(aes(x = floor, y = sum_cases, fill = ifelse(floor >= "2021-01-01", TRUE, FALSE))) +
    geom_col(color = "black") + 
    geom_text(aes(label = format(sum_cases, big.mark = ",", scientific = FALSE)), 
              position = position_dodge(width = 1), 
              vjust = -0.25, 
              family = "Roboto Condensed") + 
    scale_x_date(breaks = date_breaks("2 months"), labels = date_format("%b %y")) + 
    scale_y_continuous(breaks = , labels = comma) + 
    scale_fill_manual(values = c("grey", "red")) +
    labs(
      x = "", 
      y = "Casos sospechosos por mes", 
      title = "Casos sospechosos acumulados de coronavirus por mes en Nicaragua",
      subtitle = "Datos actualizados al 6 de enero de 2021",
      caption = "Datos: Observatorio Ciudadano COVID-19 | Plot: @rrmaximiliano"
    ) +   
    theme_ipsum_rc() + 
    theme(
      legend.position = "none", 
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )

ggsave("figs/cases.png", dpi = "retina", height = 8, width = 12, scale = 0.8)

# Deaths
df_country %>% 
  mutate(
    cases2 = cases - lag(cases), 
    deaths2 = deaths - lag(deaths)
  ) %>% 
  mutate(
    cases2 = ifelse(date == "2020-03-18", cases, cases2),
    deaths2 = ifelse(date == "2020-03-18", deaths, deaths2)
  ) %>% 
  mutate(
    month = zoo::as.yearmon(date),
    floor = floor_date(date, "month")
  ) %>% 
  group_by(floor) %>% 
  summarize(
    sum_cases = sum(cases2),
    sum_deaths = sum(deaths2)
  ) %>% 
  ggplot(aes(x = floor, y = sum_deaths, fill = ifelse(floor > "2021-01-01", TRUE, FALSE))) +
  geom_col(color = "black") + 
  geom_text(aes(label = format(sum_deaths, big.mark = ",", scientific = FALSE)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.25, 
            family = "Roboto Condensed") + 
  scale_x_date(breaks = date_breaks("2 months"), labels = date_format("%b %y")) + 
  scale_y_continuous(breaks = , labels = comma) + 
  scale_fill_manual(values = c("grey", "red")) +
  labs(
    x = "", 
    y = "Muertes sospechosas por mes", 
    title = "Muertes sospechosas acumuladas de coronavirus por mes en Nicaragua",
    subtitle = "Datos actualizados al 6 de enero de 2021",
    caption = "Datos: Observatorio Ciudadano COVID-19 | Plot: @rrmaximiliano"
  ) +   
  theme_ipsum_rc() + 
  theme(
    legend.position = "none", 
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

ggsave("figs/deaths.png", dpi = "retina", height = 8, width = 12, scale = 0.8)

