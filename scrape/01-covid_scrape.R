
# Packages ----------------------------------------------------------------

library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(here)
library(zoo)

# URL and Query -----------------------------------------------------------

url <- "https://wabi-north-europe-api.analysis.windows.net/public/reports/querydata?synchronous=true"

query <- readLines(here("query", "cases.txt"))


# Requesting data ------------------------------------------------------------

content_raw <- httr::POST(url, body = query, encode = "json", httr::add_headers(`X-PowerBI-ResourceKey` = "46a60415-b4e9-4b80-acf2-90a4cee42736", `Content-Type` = "application/json")) %>% content(as = 'text')

content <- jsonlite::fromJSON(content_raw) 

# Cleaning data 

data <- content$results$result$data$dsr$DS %>% .[[1]] %>% 
	.$PH %>% .[[1]] %>% .$DM0 %>% 
	.[[1]] %>% .$C 

# Using purr to get dataset
casos <- map_dfr(data, ~ as.data.frame(t(.))) %>% 
	mutate(
		date = as.POSIXct(V1/1000, origin="1970-01-01"),
		date = as_date(date)
	) %>% 
	rename(acumulado = V2, casos = V3) %>% 
	select(date, acumulado, casos) %>% 
	as_tibble()

	# Save dataset 

casos %>% 
	write_csv(here("data", "final", "covid_cases_nic.csv")) 

casos %>% 
	write_rds(here("data", "final", "covid_cases_nic.Rds")) 

# Deaths ------------------------------------------------------------------

query <- readLines(here("query", "deaths.txt"))

# Requesting data 

content_raw <- httr::POST(url, body = query, encode = "json", httr::add_headers(`X-PowerBI-ResourceKey` = "46a60415-b4e9-4b80-acf2-90a4cee42736", `Content-Type` = "application/json")) %>% content(as = 'text')

content <- jsonlite::fromJSON(content_raw) 

# Cleaning data 

data <- content$results$result$data$dsr$DS %>% .[[1]] %>% 
	.$PH %>% .[[1]] %>% .$DM0 %>% 
	.[[1]] %>% .$C 

# Using purr to get dataset
deaths <- map_dfr(data, ~ as.data.frame(t(.))) %>% 
	mutate(
		date = as.POSIXct(V1/1000, origin="1970-01-01"),
		date = as_date(date)
	) %>% 
	rename(acumulado = V2, muertes = V3) %>% 
	select(date, acumulado, muertes) %>% 
	as_tibble()

# Save dataset

deaths %>% 
	write_csv(here("data", "final", "covid_deaths_nic.csv")) 

deaths %>% 
	write_rds(here("data", "final", "covid_deaths_nic.Rds")) 

# Merge datasets ----------------------------------------------------------

df <- casos %>%  
	select(date, cases = acumulado) %>% 
	left_join(deaths %>% select(date, deaths = acumulado), by = "date") %>% 
	mutate(deaths = na.locf(deaths, na.rm = FALSE),
				 deaths = ifelse(is.na(deaths), 0, deaths)) 


# Save dataset ------------------------------------------------------------

df %>% 
	write_csv(here("data", "final", "observatorio_nicaragua.csv")) 

df %>% 
	write_rds(here("data", "final", "observatorio_nicaragua.Rds")) 

