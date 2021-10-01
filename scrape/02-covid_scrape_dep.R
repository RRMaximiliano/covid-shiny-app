
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

files <- list.files(path = here("query", "dep", "casos"),
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
	
	df <- map_dfr(data, ~ as.data.frame(t(.))) %>% 
		mutate(
			date = as.POSIXct(V1/1000, origin="1970-01-02"),
			date = as_date(date),
		) %>% 
		select(date, cases = V2) %>% 
		as_tibble() 
		
	return(df)
	
}

# Bind the rows

df <- map(queries, postfunction) 
df <- bind_rows(df, .id = "id")

df <- df %>% 
	mutate(
		departamento = case_when(
			id == 1 ~ "Boaco",
			id == 2 ~ "Carazo",
			id == 3 ~ "Chinandega",
			id == 4 ~ "Chontales",
			id == 5 ~ "Esteli",
			id == 6 ~ "Granada",
			id == 7 ~ "Jinotega",
			id == 8 ~ "Leon",
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
	select(departamento, date, cases) %>% 
	group_by(departamento) %>% 
	mutate(
		cases = ifelse(is.na(cases), lag(cases), cases)
	) %>% 
	filter(!is.na(cases))

df %>% 
	write_csv(here("data", "final", "observatorio_nicaragua_dep_cases.csv")) 

df %>% 
	write_rds(here("data", "final", "observatorio_nicaragua_dep_cases.Rds")) 
