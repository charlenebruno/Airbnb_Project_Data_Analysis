library(dplyr)
library(stringr)
library(ggplot2)
library(data.table)


setwd("C:/Users/user/Documents/ING5/Data Analytics/Projet/Airbnb_Project_Data_Analysis/App")

# a generic function to prepare data for a specific city, data_date
prepare_data <- function(country,city, data_date)
{
  # Cleaning listings dataframe

  #We get the list of urls
  urls_path <- file.path("all_data_urls.csv")
  print(paste0("reading data from ", urls_path))
  urls_data <- read.csv(urls_path)
  
  #We split the url to add columns to filter later
  url_decomposed <- str_split(urls_data$listings_data_url,"/",simplify = TRUE)
  
  urls_data$country <- url_decomposed[,4]
  urls_data$city <- url_decomposed[,6]
  urls_data$date <- url_decomposed[,7]
  
  #We filter the urls on countries and dates
  urls_data <-urls_data %>%
    filter(country %in% c("spain","france","italy","belgium")) %>%
    arrange(country)
  urls_data <-slice_head(urls_data%>%group_by(city), n = 5)
  urls_data
  # 
  # # suppose raw data is stored in all_data_urls.csv.gz
  # listings_url <- file.path("all_data_urls.csv.gz")
  # 
  # print(paste0("reading data from ", listings_url))
  # listings <- read.csv(gzfile(listings_url))
  # 
  # ## Add Keys: columns country,city and day date
  # listings$country <- country
  # listings$city <- city
  # listings$data_date <- data_date
  # 
  # ## Select interesting columns
  # ### Most columns don't contain interesting information
  # columns_listings <- c("country","city", "data_date", "id", "neighbourhood_cleansed", 
  #                       "latitude", "longitude", 
  #                       "property_type", "room_type", "accommodates", "bedrooms", 
  #                       "beds", "price", "minimum_nights",  "maximum_nights")
  # 
  # listings <- listings %>% 
  #   select(columns_listings) %>% 
  #   arrange(id)
  
  
  # # Cleaning calendar dataframe
  # 
  # ## arrange by id and date
  # calendar <- calendar %>% 
  #   arrange(listing_id, date)
  # 
  # ## add day number (starting first day)
  # calendar <- calendar %>%
  #   group_by(listing_id) %>%
  #   mutate(day_nb = row_number()) %>%
  #   ungroup()
  # 
  # ## change available column to binary
  # calendar <- calendar %>%
  #   mutate(available = ifelse(available=="t", 1, 0))
  # 
  # ## clean price column and transform to numeric
  # calendar <- calendar %>%
  #   mutate(price = str_replace(price, "\\$", ""),
  #          adjusted_price = str_replace(adjusted_price, "\\$", ""))
  # calendar <- calendar %>%
  #   mutate(price = str_replace(price, ",", ""),
  #          adjusted_price = str_replace(adjusted_price, ",", ""))
  # calendar <- calendar %>%
  #   mutate(price = as.numeric(price),
  #          adjusted_price = as.numeric(adjusted_price))
  
  # ## calculate estimated revenue for upcoming day
  # calendar <- calendar %>%
  #   mutate(revenue = price*(1-available))
  
  ## calculate availability, price, revenue for next 30, 60 days ... for each listing_id
  # calendar <- calendar %>%
  #   group_by(listing_id) %>%
  #   summarise(availability_30 = sum(available[day_nb<=30], na.rm = TRUE),
  #             price_30 = mean(price[day_nb<=30 & available==0], na.rm = TRUE),
  #             revenue_30 = sum(revenue[day_nb<=30], na.rm = TRUE),
  #   )
  
  
  # dir.create(file.path("data_cleansed", city, data_date), recursive = TRUE)
  # 
  # #write the cleansed data in csv
  # write.csv(listings, file.path("data_cleansed", country,city, data_date, "listings.csv"))
  # print(paste0("saving data into ", file.path("data_cleansed", country,city, data_date, "listings.csv")))
  # 
}  

#Test sur un pays, une ville, une date
country<- "spain"
city <- "barcelona"
data_date <- "2020-10-12"
prepare_data(country,city, data_date)


# Example: Prepare data for multiple cities
# countries <- c("france","spain","italy")
# cities <- c("malaga", "mallorca", "sevilla")
# data_dates <- c("2020-06-30", "2020-09-19", "2020-06-29")
# for(i in 1:length((countries))){
#     for(i in 1:length(cities)){
#     country <- countries[i]
#     city <- cities[i]
#     data_date <- data_dates[i]
#     print("-------------------------------------------------")
#     print(paste(c("Preparing data for", country,city, "compiled at", data_date), collapse = " "))
#     prepare_data(country,city, data_date)
#   }
# }

# Clean Environment
#rm(list=ls())

## Once data for multiple cities are prepared
## We can read these data and concatenate them together into one dataframe

# # Reading cleansed data
# cities <- c("malaga", "mallorca", "sevilla")
# data_dates <- c("2020-06-30", "2020-09-19", "2020-06-29")
# 
# # We are only interested in data between min_date and max_date
# min_date <- '2020-05-01'
# max_date <- '2020-11-01'
# 
# files_paths <- c()
# 
# # Read data in cities between min_date and max_date
# for(city in cities){
#   file_dir <- file.path(".", "data_cleansed", city)
#   file_subdirs <- list.dirs(file_dir)
#   file_subdirs <- file_subdirs[-1]
#   
#   for(file_subdir in file_subdirs){
#     if(file_subdir < file.path(file_dir, min_date) | file_subdir > file.path(file_dir, max_date)  )
#       file_subdirs = file_subdirs[file_subdirs != file_subdir]
#   }
#   files_paths <- c(files_paths, file_subdirs)
# }
# files_paths <- file.path(files_paths, "listings.csv")
# listings <- 
#   do.call(rbind,
#           lapply(files_paths, read.csv, row.names=1))
# 
# ## Preprocess
# listings$bedrooms <- ifelse(listings$bedrooms >= 5, "5+", listings$bedrooms)



