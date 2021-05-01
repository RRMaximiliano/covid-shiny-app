
# Packages ----------------------------------------------------------------

library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(here)
library(zoo)

# Function ----------------------------------------------------------------

# URLS and queries

url <- "https://wabi-north-europe-api.analysis.windows.net/public/reports/querydata?synchronous=true"

files <- list.files(path = here("query", "dep", "muertes"),
										pattern = "*.txt", full.names = TRUE)

queries <- lapply(files, readLines)


# Create the function

if(exists("df")){
	rm(df)
}

postfunction <- function(sym) {
	
	content_raw <- httr::POST(url, body = sym, encode = "json", 
														httr::add_headers(`X-PowerBI-ResourceKey` = "46a60415-b4e9-4b80-acf2-90a4cee42736", 
																							`Content-Type` = "application/json")) %>% content(as = 'text')
	content <- jsonlite::fromJSON(content_raw)
	
	data <- content$results$result$data$dsr$DS %>% .[[1]] %>% 
		.$PH %>% .[[1]] %>% .$DM0 %>% 
		.[[1]] %>% .$C 
	
	df <- map_dfr(data, ~ as.data.frame(t(.))) 
	
	return(df)
	
}

# Bind the rows

muertes <- map(queries, postfunction) 
muertes <- bind_rows(muertes, .id = "id")

muertes <- muertes %>% 
	group_by(id) %>% 
	mutate(
		date = as.POSIXct(V1/1000, origin="1970-01-01"),
		date = as_date(date), 
		V2 = na.locf(V2, na.rm = FALSE)
	) %>% 
	ungroup() %>% 
	mutate(
		departamento = case_when(
			id == 1 ~ "Boaco",
			id == 2 ~ "Carazo",
			id == 3 ~ "Chinandega",
			id == 4 ~ "Chontales",
			id == 5 ~ "Estelí",
			id == 6 ~ "Granada",
			id == 7 ~ "Jinotega",
			id == 8 ~ "León",
			id == 9 ~ "Madriz",
			id == 10 ~ "Managua",
			id == 11 ~ "Masaya",
			id == 12 ~ "Matagalpa",
			id == 13 ~ "Nueva Segovia",
			id == 14 ~ "RACCN",
			id == 15 ~ "RACCS",
			id == 16 ~ "Rio San Juan",
			id == 17 ~ "Rivas",		
		)
	) %>% 
	select(departamento, date, deaths = V2)

muertes %>% 
	write_csv(here("data", "final", "observatorio_nicaragua_dep_deaths.csv")) 

muertes %>% 
	write_rds(here("data", "final", "observatorio_nicaragua_dep_deaths.Rds")) 


# Merge with cases to get full dep data -----------------------------------

# Merge with cases

cases <- read_rds(here("data", "final", "observatorio_nicaragua_dep_cases.Rds"))

full_df <- cases %>% 
	left_join(muertes, by = c("departamento", "date")) %>% 
	group_by(departamento) %>% 
	mutate(
		deaths = ifelse(is.na(deaths), 0, deaths)
	) %>% 
	ungroup()

full_df %>% 
	write_csv(here("data", "final", "observatorio_nicaragua_dep.csv")) 

full_df %>% 
	write_rds(here("data", "final", "observatorio_nicaragua_dep.Rds")) 
