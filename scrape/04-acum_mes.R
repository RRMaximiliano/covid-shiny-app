
# Packages ----------------------------------------------------------------

library(lubridate)
library(hrbrthemes)
library(tidyverse)
library(lubridate)
library(scales)
library(ggtext)
library(here)


# Load df -----------------------------------------------------------------

df_country <- read_csv(here("data", "final", "observatorio_nicaragua.csv"))

Sys.setlocale("LC_TIME", "Spanish_Spain.1252")
roboto = "Roboto Condensed"

## Cases ----

max_date <- max(df_country$date)

format(max_date, "%b %d, %Y")

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
    sum_cases = sum(cases2, na.rm = TRUE),
    sum_deaths = sum(deaths2, na.rm = TRUE)
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
      caption = paste0("Datos: Observatorio Ciudadano COVID-19, Nicaragua\nÚltimo día de actualización: ", format(max_date, "%B %d, %Y"), "\nPlot: @rrmaximiliano")
    ) +   
  theme_ipsum_rc(base_size = 14) +
  theme(
    axis.title.x = element_text(hjust = 0.5, size = 12),
    axis.title.y = element_text(hjust = 0.5, size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(), 
    # plot.title = element_markdown(),
    legend.position = "none",
    # legend.title = element_text(family = roboto)
  )

ggsave(here("plots", "cases.png"),
       dpi = 320, height = 10, width = 20, scale = 0.8, bg = "white")

## Deaths ----
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
    sum_cases = sum(cases2, na.rm = TRUE),
    sum_deaths = sum(deaths2, na.rm = TRUE)
  ) %>% 
  ggplot(aes(x = floor, y = sum_deaths, fill = ifelse(floor >= "2021-01-01", TRUE, FALSE))) +
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
    caption = paste0("Datos: Observatorio Ciudadano COVID-19, Nicaragua\nÚltimo día de actualización: ", format(max_date, "%B %d, %Y"), "\nPlot: @rrmaximiliano")
  ) +   
  theme_ipsum_rc(base_size = 14) +
  theme(
    axis.title.x = element_text(hjust = 0.5, size = 12),
    axis.title.y = element_text(hjust = 0.5, size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(), 
    # plot.title = element_markdown(),
    legend.position = "none",
    # legend.title = element_text(family = roboto)
  )

ggsave(here("plots", "deaths.png"),
       dpi = 320, height = 10, width = 20, scale = 0.8, bg = "white")

