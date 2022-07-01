# Import libraries
library(tidyr)
library(ggplot2)
library(readr)

# Read in data
airbnb <- read.csv('airbnb_listings.csv')

se_med_rent_studio <- read.csv('medianAskingRent_Studio.csv')
se_med_rent_OneBd <- read.csv('medianAskingRent_OneBd.csv')
se_med_rent_TwoBd <- read.csv('medianAskingRent_TwoBd.csv')
se_med_rent_ThreePlusBd <- read.csv('medianAskingRent_ThreePlusBd.csv')
se_med_rent_all <- read.csv('medianAskingRent_All.csv')
se_inventory <- read.csv('rentalInventory_All.csv')

airbnb_subset <- subset(airbnb, select = c('id', 'listing_url', 'last_scraped',
                                           "neighbourhood_cleansed", 
                                           "neighbourhood_group_cleansed",
                                           "latitude", "longitude", 
                                           "property_type", 'room_type',
                                           'accommodates', 'bathrooms_text',
                                           'bedrooms', 'beds', 'price',
                                           'minimum_nights', 'maximum_nights',
                                           "availability_365"))

airbnb_subset <- subset(airbnb_subset, 
                        airbnb_subset$room_type == 'Entire home/apt' & 
                          airbnb_subset$bedrooms <= 2) %>% 
  drop_na(bedrooms, bathrooms_text, beds, price)

airbnb_subset$price <- parse_number(airbnb_subset$price)

summary(airbnb_subset$minimum_nights)

airbnb_subset_long_term <- subset(airbnb_subset, 
                                  airbnb_subset$minimum_nights >= 28 &
                                    airbnb_subset$availability_365 >= 28)

  
ggplot(data = airbnb_subset_long_term, aes(x=neighbourhood_cleansed, y = price)) +
  geom_boxplot() + facet_wrap(vars(neighbourhood_group_cleansed), scales = "free") + 
  theme(axis.text.x = element_text(angle = 90)) + coord_flip()
