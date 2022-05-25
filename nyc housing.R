library(tidyr)

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

airbnb_subset <- subset(airbnb_subset, airbnb_subset$room_type == 'Entire home/apt')

airbnb_subset <- airbnb_subset %>% drop_na(bedrooms, bathrooms_text, beds,
                                           price)

airbnb_subset <- subset(airbnb_subset, airbnb_subset$bedrooms <= 2)
