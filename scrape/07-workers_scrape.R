
# Packages ----------------------------------------------------------------

library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(here)
library(zoo)

# URL and Query -----------------------------------------------------------

url <- "https://wabi-north-europe-api.analysis.windows.net/public/reports/querydata?synchronous=true"

query <- readLines(here("query", "workers.txt"))

content_raw <- httr::POST(url, body = query, encode = "json", httr::add_headers(`X-PowerBI-ResourceKey` = "46a60415-b4e9-4b80-acf2-90a4cee42736", `Content-Type` = "application/json")) %>% content(as = 'text')

content <- jsonlite::fromJSON(content_raw) 


# Cleaning data 
data <- content$results$result$data$dsr$DS %>% .[[1]] %>% 
	.$PH %>% .[[1]] %>% .$DM0 %>% 
	.[[1]] %>% .$C 

# Using purr to get dataset
df <- map_dfr(data, ~ as_tibble(t(.))) %>% 
	mutate(
		date = as.POSIXct(V1/1000, origin = "1970-01-01"),
		date = as_date(date)
	) %>% 
	rename(cases = V2, deaths = V3) %>% 
	select(date, cases, deaths) %>% 
	as_tibble()

df <- df %>% 
	mutate(
		cases = na.locf(cases, na.rm = FALSE),
		deaths = na.locf(deaths, na.rm = FALSE),
		deaths = ifelse(is.na(deaths), 0, deaths),
		# cases = ifelse(lead(cases) < cases, NA_integer_, cases), 
	) 

# Fix some wrong listing numbers
df <- df %>% 
	mutate(
		cases2 = ifelse(lag(cases) > cases, NA_integer_, cases),
		cases2 = na.locf(cases2, na.rm = FALSE),
		cases2 = ifelse(lag(cases2) > cases2, NA_integer_, cases2),
		cases2 = na.locf(cases2, na.rm = FALSE),
		cases2 = ifelse(is.na(cases2), cases, cases2)
	) %>% 
	select(date, cases = cases2, deaths)

# Save dataset ------------------------------------------------------------

df %>% 
	write_csv(here("data", "final", "health_workers.csv")) 

df %>% 
	write_rds(here("data", "final", "health_workers.Rds")) 
