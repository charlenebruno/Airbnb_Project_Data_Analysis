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
urls_data

#Load data from selected urls
for(i in 1:nrow(urls_data)){
  prepare_data(urls_data[i,]$listings_data_url,urls_data[i,]$country,urls_data[i,]$city,urls_data[i,]$date)
}





