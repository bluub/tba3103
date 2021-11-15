# This will output 3 distinct files:
# 1. cases: eu_ecdc_gisaid_cases.csv
# 2. variants: eu_ecdc_gisaid_variants.csv
# 3. sequences: eu_ecdc_gisaid_sequences.csv


#### Load libraries & Setup ----
# load dplyr for distinct, summary, etc
library(dplyr)

# load tidyr library for the unnest function
library(tidyr)

# load tsibble library because we need to check if a year has 53 weeks
library(tsibble)

# Set the location to the /var/www/csv folder so that we don't need to move it afterwards
setwd('/var/www/csv')
# For development purposes, please comment the above and uncomment the following
# setwd('~')


#### Set local constants ----
# Set a key value list to map a country to their geographic region
# based on UN M49 (https://unstats.un.org/unsd/methodology/m49/)
eu_country_regions <- list(
  Austria = "West",
  Belgium = "West",
  Bulgaria = "East",
  Croatia = "South",
  Cyprus= "South",
  Czechia = "East",
  Denmark = "North",
  Estonia = "North",
  Finland = "North",
  France = "West",
  Germany = "West",
  Greece = "South",
  Hungary = "East",
  Iceland = "North",
  Ireland = "North",
  Italy = "South",
  Latvia = "North",
  Liechtenstein = "West",
  Lithuania = "North",
  Luxembourg = "West",
  Malta = "South",
  Netherlands = "West",
  Norway = "North",
  Poland = "East",
  Portugal = "South",
  Romania = "East",
  Slovakia = "East",
  Slovenia = "South",
  Spain = "South",
  Sweden = "North"
)

# regions latitude & longitude as lists
eu_region_lat <- list(
  North = "60.472024",
  South = "41.87194",
  East = "47.9495",
  West = "48.5734"
)

eu_region_long <- list(
  North = "8.468946",
  South = "12.56738",
  East = "21.7244",
  West = "7.7521"
)

# countries latitude & longitude as lists
eu_country_capital_lat <- list(
  Austria = "47.516231",
  Belgium = "50.503887",
  Bulgaria = "42.733883",
  Croatia = "45.1",
  Cyprus= "35.126413",
  Czechia = "49.817492",
  Denmark = "56.26392",
  Estonia = "58.595272",
  Finland = "61.92411",
  France = "46.227638",
  Germany = "51.165691",
  Greece = "39.074208",
  Hungary = "47.162494",
  Iceland = "64.963051",
  Ireland = "53.41291",
  Italy = "41.87194",
  Latvia = "56.879635",
  Liechtenstein = "47.166",
  Lithuania = "55.169438",
  Luxembourg = "49.815273",
  Malta = "35.937496",
  Netherlands = "52.132633",
  Norway = "60.472024",
  Poland = "51.919438",
  Portugal = "39.399872",
  Romania = "45.943161",
  Slovakia = "48.669026",
  Slovenia = "46.151241",
  Spain = "40.463667",
  Sweden = "60.128161"
)

eu_country_capital_long <- list(
  Austria = "14.550072",
  Belgium = "4.469936",
  Bulgaria = "25.48583",
  Croatia = "15.2",
  Cyprus= "33.429859",
  Czechia = "15.472962",
  Denmark = "9.501785",
  Estonia = "25.013607",
  Finland = "25.748151",
  France = "2.213749",
  Germany = "10.451526",
  Greece = "21.824312",
  Hungary = "19.503304",
  Iceland = "-19.020835",
  Ireland = "-8.24389	",
  Italy = "12.56738",
  Latvia = "24.603189",
  Liechtenstein = "9.555373",
  Lithuania = "23.881275",
  Luxembourg = "6.129583",
  Malta = "14.375416",
  Netherlands = "5.291266",
  Norway = "8.468946",
  Poland = "19.145136",
  Portugal = "-8.224454",
  Romania = "24.96676",
  Slovakia = "19.699024",
  Slovenia = "14.995463",
  Spain = "-3.74922",
  Sweden = "18.643501"
)


#### Set local functions ----
get_date_from_year_week <- function(year_week){
  # Because the way that the dataset behaves, it is possible to have 53 weeks in a year,
  #   but the as.Date function in R does not take that into account.
  # So, we have this "hacky" function to check whether the year has 53 weeks & "subtract" a week if they do
  year <- as.integer(strsplit(year_week, "-")[[1]][1])
  week <- as.integer(strsplit(year_week, "-")[[1]][2])
  
  if(is_53weeks(year)){
    week <- week - 1
  }
  
  return(strftime(as.Date(paste0(year, sprintf("%02d", week), 1), "%Y%U%u"), "%Y-%m-%d"))
}


#### Transform the existing fields in the csv ----
# Download the base csv from ECDC's website
data <- read.csv('https://opendata.ecdc.europa.eu/covid19/virusvariant/csv/data.csv', stringsAsFactors = FALSE)

# Drop all non-GISAID sources
data <- data[data$source == 'GISAID',]

# Add a new field to change the year-week to the date of the monday
data$date_monday <- lapply(data$year_week, get_date_from_year_week)
data <- unnest(data, date_monday)

# Add 2 new fields which are the countries' latitude & longitude
data$capital_lat <- eu_country_capital_lat[data$country]
data <- unnest(data, capital_lat)
data$capital_lng <- eu_country_capital_long[data$country]
data <- unnest(data, capital_lng)

# Add a new field for the countries' region
data$region <- eu_country_regions[data$country]
data <- unnest(data, region)

# Add 2 new fields which are the regions' latitude & longitude
data$region_lat <- eu_region_lat[data$region]
data <- unnest(data, region_lat)
data$region_lng <- eu_region_long[data$region]
data <- unnest(data, region_lng)


#### Build the dataframe for cases ----
# Create a new dataframe & only pick the data we want for cases
data_cases <- data.frame(
  country = data$country, 
  capital_lat = data$capital_lat,
  capital_lng = data$capital_lng,
  region = data$region,
  region_lat = data$region_lat,
  region_lng = data$region_lng,
  date_monday = data$date_monday,
  new_cases = data$new_cases,
  stringsAsFactors = FALSE
)

# Since the country, year-week, and new_cases are duplicated due to the number of variants,
# we'll just grab the distinct values of those by running the following.
# 
# As of the time of running this, there a a total of 21 "variants", so it should shrink by around that amount
data_cases <- data_cases %>%
  distinct(country, date_monday, .keep_all = TRUE)

# Write to the csv file
write.csv(data_cases, 'eu_ecdc_gisaid_cases.csv', row.names = FALSE)



#### Build the dataframe for variants ----
# Create a new dataframe & only pick the data we want for variants
data_variants <- data.frame(
  country = data$country, 
  region = data$region,
  date_monday = data$date_monday,
  variant = data$variant,
  number_detections_variant = data$number_detections_variant,
  stringsAsFactors = FALSE
)

# Remove the rows that have 0 variants detected, since that is uselessly bloated data
# With the lack of 0 variants detected, we can also find out when was the earliest date a variant was sequenced
data_variants <- data_variants[data_variants$number_detections_variant != 0,]

# Write to the csv file
write.csv(data_variants, 'eu_ecdc_gisaid_variants.csv', row.names = FALSE)


#### Build the dataframe for sequences ----
# Create a new dataframe & only pick the data we want for sequences
data_sequences <- data.frame(
  country = data$country, 
  region = data$region,
  date_monday = data$date_monday,
  new_cases = data$new_cases,
  number_sequenced = data$number_sequenced,
  variant = data$variant,
  number_detections_variant = data$number_detections_variant,
  stringsAsFactors = FALSE
)

# Categorise the sequenced variants into 3 distinct columns:
#   1. known_variants (when the variant is neither "UNK" nor "Other")
#   2. unknown_variants (when the variant is marked as "UNK")
#   3. other_variants (when the variant is marked as "Other")
data_sequences <- data_sequences %>%
  group_by(country, region, date_monday, new_cases, number_sequenced) %>%
  summarise(known_variants = sum(number_detections_variant[!variant %in% (c('UNK', 'Other'))]),
            unknown_variants = sum(number_detections_variant[variant == 'UNK']),
            other_variants = sum(number_detections_variant[variant == 'Other']))

# Write to the csv file
write.csv(data_sequences, 'eu_ecdc_gisaid_sequences.csv', row.names = FALSE)

