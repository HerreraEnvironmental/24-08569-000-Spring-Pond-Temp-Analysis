## Required Packages
library(arrow)
library(tidyverse)
library(httr2)
library(jsonlite)
library(dotenv)

# functions ---------------------------------
# column names
DRIP_ts_columns <- function(secret, project){
  response <- request("https://hec-drip-edms.azurewebsites.net/ts") %>%
    req_headers("accept" = "application/json") %>%
    req_headers("x-api-key" = secret) %>%      # API key
    req_url_path_append(project) %>%           # Aquarius project number
    req_url_path_append("table_columns")
  
  table_info <- response %>%
    req_timeout(30) %>%
    req_perform() %>%
    resp_body_string() %>%
    fromJSON()

  data.frame(table_info$results) 
}
# data
DRIP_ts_parquet <- function(secret, project, start_date = Sys.Date()-years(1), end_date = Sys.Date(), columns, nested = FALSE){
  response <- request("https://hec-drip-edms.azurewebsites.net/ts") %>%
    req_headers("accept" = "application/json") %>%
    req_headers("x-api-key" = secret) %>%        # API key
    req_url_path_append(project) %>%             # Aquarius project number
    req_url_path_append("parquet") %>%
    req_url_query("start_date" = start_date) %>% # default is one year prior to current date 
    req_url_query("end_date" = end_date) %>%     # default is current date
    req_url_query("timezone" = "US/Pacific") %>%
    req_url_query("columns" = columns, .multi = "explode") %>%
    req_url_query("nested" = nested)
  
  # REQUEST PERFORM AND TRANSFORM TO JSON
  table_info <- response %>%
    req_timeout(30) %>%
    req_perform()
  
  # final output from API
  read_parquet(table_info$body)
}

# Query ------------------------------------------------------------------
# Have .env file with API key in your repository and run the following 2 lines
dotenv::load_dot_env()
secret <- Sys.getenv("EDMS_API_KEY")
project <- "24-08569-000"
# get column names
ts_columns <- DRIP_ts_columns(secret, project)
ts_column_names <- ts_columns$column[ts_columns$parameter=="Water Temp"]

# query data
raw_temp_data <- DRIP_ts_parquet(secret, project, columns = ts_column_names)

temp_data <- raw_temp_data %>%
  pivot_longer(cols = c(2:5), values_to = "Value") %>%
  mutate(name = gsub("Water Temp_degC_Spring Pond_", "", name), 
    Locator = ifelse(name == "SPING-NORTH", "SPRING-NORTH", name),
    .keep = "unused") %>%
  filter(!is.na(Value))