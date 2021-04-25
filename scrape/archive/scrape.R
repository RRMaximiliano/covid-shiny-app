
# Load packages -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load(dplyr, purrr, readr, wdman, RSelenium, xml2, selectr)

# Start and open Firefox --------------------------------------------------
selServ <- selenium(
  port = 4444L,
  version = 'latest',
  chromever = '88.0.4324.96',
)

# Start firefox
remDr <- remoteDriver(
  remoteServerAddr = 'localhost',
  port = 4444L,
  browserName = 'firefox'
)

# Open firefox
remDr$open()

# Navigate to the powerbi app ---------------------------------------------

# navigate to PowerBI APP
covid_url <- "https://app.powerbi.com/view?r=eyJrIjoiNDZhNjA0MTUtYjRlOS00YjgwLWFjZjItOTBhNGNlZTQyNzM2IiwidCI6ImM0YmUwZDY5LTM2ZjgtNGJhMi1hYTk3LWMxMTM1ZGI1NGYzYyIsImMiOjh9&amp;pageName=ReportSectiond8171dcd4c587890996e"
remDr$navigate(covid_url)
Sys.sleep(5)

# Move to page 2 of 8
category <- remDr$findElement("xpath", "/html/body/div[1]/root/div/div/div[2]/logo-bar/div/div/div/logo-bar-navigation/span/a[3]/i")
category$clickElement()

# Problem here
category <- remDr$findElement(using = "css selector", "div.lineChart")
category$clickElement()
rem

# Getting the table
covid_data <- read_html(remDr$getPageSource()[[1]]) %>%
  querySelector("div.pivotTable")

col_headers <- covid_data %>%
  querySelectorAll("div.columnHeaders div.pivotTableCellWrap") %>%
  map_chr(xml_text)

rownames <- covid_data %>%
  querySelectorAll("div.rowHeaders div.pivotTableCellWrap") %>%
  map_chr(xml_text)

covid_df <- covid_data %>%
  querySelectorAll("div.bodyCells div.pivotTableCellWrap") %>%
  map(xml_parent) %>%
  unique() %>%
  map(~ .x %>% querySelectorAll("div.pivotTableCellWrap") %>% map_chr(xml_text)) %>%
  setNames(col_headers) %>%
  bind_cols()

# Get final data ----------------------------------------------------------

# Get the final data
df_final <- tibble(covid = rownames, covid_df) %>%
  type_convert(trim_ws = T, na = c(""))

