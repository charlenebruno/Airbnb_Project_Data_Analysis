library(dplyr)
library(stringr)
library(ggplot2)
library(data.table)


setwd("C:/Users/user/Documents/ING5/Data Analytics/Projet/Airbnb_Project_Data_Analysis/App")

# a generic function to prepare data for a specific url,country,city, date
prepare_data <- function(url,country,city,date)
{
  # Cleaning listings dataframe
  
  #We load the data from the http link
  tmp <- tempfile()
  download.file(url,tmp)
  listings <- read.csv(gzfile(tmp))
  
  #Read data online directly in R:
  #read.csv(textConnection(readLines(gzcon(url(url_name)))))
 
  ## Add Keys: columns country,city and day date
  listings$country <- country
  listings$city <- city
  listings$date <- date
  
  # ## Select interesting columns
  columns_listings <- c("country","city", "date", "id", "neighbourhood_cleansed", 
                         "latitude", "longitude", 
                        "property_type", "room_type", "accommodates", "bedrooms", 
                        "beds", "price", "minimum_nights",  "maximum_nights","availability_30")
   
   listings <- listings %>% 
     select(which(names(.) %in% columns_listings)) %>% 
      arrange(id)

  ## clean price column and transform to numeric
   listings$price <- as.numeric(str_remove(listings$price,"[$]"))
   listings$availability_30 <- as.numeric(listings$availability_30)
   listings$availability_30[is.na(listings$availability_30)]<-30
   ##Add revenue_30
   listings$revenue_30 <- listings$price *(30-listings$availability_30)
   listings$revenue_30[is.na(listings$revenue_30)]<-0
  
  #write the cleansed data in csv
  dir.create(file.path("data_cleansed",country, city, date), recursive = TRUE)
  write.csv(listings, file.path("data_cleansed", country,city, date, "listings.csv"))
  print(paste0("saving data into ", file.path("data_cleansed", country,city,date, "listings.csv")))

}  

decompose__filtered_urls <-function(){
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
  #filter countries: Spain, France and Belgium
  urls_data <-urls_data %>%
    filter(country %in% c("france","spain","germany")) %>%
    filter(city !="paris" ) %>%
    arrange(country)
  #Paris 
  #filter dates: 3 first available dates 
  urls_data <-slice_head(urls_data%>%group_by(city), n = 3)
  return(urls_data) 
}

clean_all_data <- function(){
  urls_data <- decompose__filtered_urls()
  #Load data from selected urls
  for(i in 1:nrow(urls_data)){
    prepare_data(urls_data[i,]$listings_data_url,urls_data[i,]$country,urls_data[i,]$city,urls_data[i,]$date)
  }
}

#clean_all_data()

## Once data for multiple cities are prepared
## We can read these data and concatenate them together into one dataframe
read_cleansed_data <- function(){
  # Reading cleansed data
  countries_ <- 
  
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
  return(listings)
}




