# Import libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(ggthemes)
library(zoo)
library(pals)

# Read in data
airbnb <- read.csv('airbnb_listings.csv')

se_med_rent_studio <- read.csv('medianAskingRent_Studio.csv')
se_med_rent_OneBd <- read.csv('medianAskingRent_OneBd.csv')
se_med_rent_TwoBd <- read.csv('medianAskingRent_TwoBd.csv')
se_med_rent_ThreePlusBd <- read.csv('medianAskingRent_ThreePlusBd.csv')
se_med_rent_all <- read.csv('medianAskingRent_All.csv')
se_inventory <- read.csv('rentalInventory_All.csv')

airbnb_subset <- subset(airbnb, 
                        airbnb$room_type == 'Entire home/apt' & 
                          airbnb$bedrooms <= 2) %>% 
  drop_na(bedrooms, bathrooms_text, beds, price)

airbnb_subset$price <- parse_number(airbnb_subset$price)

airbnb_subset$neighbourhood_cleansed <- 
  ifelse(airbnb_subset$neighbourhood_cleansed == 'Gramercy', 'Gramercy Park', 
         airbnb_subset$neighbourhood_cleansed)

airbnb_subset$neighbourhood_cleansed <- 
  ifelse(airbnb_subset$neighbourhood_cleansed == 'SoHo', 'Soho', 
         airbnb_subset$neighbourhood_cleansed)

airbnb_subset$neighbourhood_cleansed <- 
  ifelse(airbnb_subset$neighbourhood_cleansed == 'Stuyvesant Town', 
         'Stuyvesant Town/PCV', 
         airbnb_subset$neighbourhood_cleansed)

airbnb_subset$neighbourhood_cleansed <- 
  ifelse(airbnb_subset$neighbourhood_cleansed == 'Prospect-Lefferts Gardens', 
         'Prospect Lefferts Gardens', 
         airbnb_subset$neighbourhood_cleansed)

airbnb_subset$neighbourhood_cleansed <- 
  ifelse(airbnb_subset$neighbourhood_cleansed == 'Columbia St', 
         'Columbia St Waterfront District', 
         airbnb_subset$neighbourhood_cleansed)

airbnb_subset$neighbourhood_cleansed <- 
  ifelse(airbnb_subset$neighbourhood_cleansed == 'Sea Gate', 
         'Seagate', 
         airbnb_subset$neighbourhood_cleansed)

# Streeteasy things
wide_to_long <- function(df) {
  new_df <- subset(df, df$areaType == 'neighborhood') %>% 
    gather(year, med_rent, X2010.01:X2022.04) %>% arrange(areaName);
  new_df$year <- gsub("X", "", as.character(new_df$year)) %>% 
    as.yearmon("%Y.%m");
  return(new_df)
}

se_med_rent_OneBd <- wide_to_long(se_med_rent_OneBd)
se_med_rent_OneBd$bedroom <- 1

se_med_rent_TwoBd <- wide_to_long(se_med_rent_TwoBd)
se_med_rent_TwoBd$bedroom <- 2

se_med_rent <- rbind(se_med_rent_OneBd, se_med_rent_TwoBd)

# more cleaning

common_neighborhoods <- intersect(unique(airbnb_subset$neighbourhood_cleansed), 
          unique(se_med_rent$areaName))

se_med_rent <- se_med_rent[se_med_rent$areaName %in% common_neighborhoods,]
# plots

#manhattan
ggplot(data = subset(se_med_rent, 
                     se_med_rent$Borough=='Manhattan' & 
                       se_med_rent$bedroom == 1), 
       aes(x=year, y=med_rent, col=areaName))+ 
  geom_line(aes(group=areaName)) + theme_clean() +
  stat_summary(
    data = ~ subset(se_med_rent, 
                    se_med_rent$Borough=='Manhattan' & 
                      se_med_rent$bedroom == 1),
    aes(group = -1), col = 'black',
    fun.data = mean_se,
    geom = "line", size = 2) + 
  scale_color_manual(values=as.vector(cols25())) +
  labs(x='Month Year', y='Median Rant', 
       title = 
         'Median Rent of 1 Bedroom Apartments in Manhattan Neighborhoods from Jan 2010 - Apr 2022',
       color='Neighborhood') + guides(color = guide_legend(nrow = 4)) +
  theme(legend.justification = c('left', 'top'),
        legend.key.size = unit(0.5, "cm"),
        legend.position = 'top')


ggplot(data = subset(se_med_rent, 
                     se_med_rent$Borough=='Manhattan' & 
                       se_med_rent$bedroom == 2), 
       aes(x=year, y=med_rent, col=areaName))+ 
  geom_line(aes(group=areaName)) + theme_clean() +
  stat_summary(
    data = ~ subset(se_med_rent, 
                    se_med_rent$Borough=='Manhattan' & 
                      se_med_rent$bedroom == 2),
    aes(group = -1), col = 'black',
    fun.data = mean_se,
    geom = "line", size = 2) + 
  scale_color_manual(values=as.vector(cols25())) +
  labs(x='Month Year', y='Median Rant', 
       title = 
         'Median Rent of 2 Bedroom Apartments in Manhattan Neighborhoods from Jan 2010 - Apr 2022',
       color='Neighborhood') + guides(color = guide_legend(nrow = 4)) +
  theme(legend.justification = c('left', 'top'),
        legend.key.size = unit(0.5, "cm"),
        legend.position = 'top')

#queens
ggplot(data = subset(se_med_rent, 
                     se_med_rent$Borough=='Queens' & 
                       se_med_rent$bedroom == 1), 
       aes(x=year, y=med_rent, col=areaName))+ 
  geom_line(aes(group=areaName)) + theme_clean() +
  stat_summary(
    data = ~ subset(se_med_rent, 
                    se_med_rent$Borough=='Queens' & 
                      se_med_rent$bedroom == 1),
    aes(group = -1), col = 'black',
    fun.data = mean_se,
    geom = "line", size = 2) + 
  scale_color_manual(values=as.vector(c(cols25(), kelly()))) +
  labs(x='Month Year', y='Median Rant', 
       title = 
         'Median Rent of 1 Bedroom Apartments in Queens Neighborhoods from Jan 2010 - Apr 2022',
       color='Neighborhood') + guides(color = guide_legend(nrow = 6)) +
  theme(legend.justification = c('left', 'top'),
        legend.key.size = unit(0.5, "cm"),
        legend.position = 'top')


ggplot(data = subset(se_med_rent, 
                     se_med_rent$Borough=='Queens' & 
                       se_med_rent$bedroom == 2), 
       aes(x=year, y=med_rent, col=areaName))+ 
  geom_line(aes(group=areaName)) + theme_clean() +
  stat_summary(
    data = ~ subset(se_med_rent, 
                    se_med_rent$Borough=='Queens' & 
                      se_med_rent$bedroom == 2),
    aes(group = -1), col = 'black',
    fun.data = mean_se,
    geom = "line", size = 2) + 
  scale_color_manual(values=as.vector(c(cols25(), kelly()))) +
  labs(x='Month Year', y='Median Rant', 
       title = 
         'Median Rent of 2 Bedroom Apartments in Queens Neighborhoods from Jan 2010 - Apr 2022',
       color='Neighborhood') + guides(color = guide_legend(nrow = 6)) +
  theme(legend.justification = c('left', 'top'),
        legend.key.size = unit(0.5, "cm"),
        legend.position = 'top')

#brooklyn
ggplot(data = subset(se_med_rent, 
                     se_med_rent$Borough=='Brooklyn' & 
                       se_med_rent$bedroom == 1), 
       aes(x=year, y=med_rent, col=areaName))+ 
  geom_line(aes(group=areaName)) + theme_clean() +
  stat_summary(
    data = ~ subset(se_med_rent, 
                    se_med_rent$Borough=='Brooklyn' & 
                      se_med_rent$bedroom == 1),
    aes(group = -1), col = 'black',
    fun.data = mean_se,
    geom = "line", size = 2) + 
  scale_color_manual(values=as.vector(c(cols25(), kelly()))) +
  labs(x='Month Year', y='Median Rant', 
       title = 
         'Median Rent of 1 Bedroom Apartments in Brooklyn Neighborhoods from Jan 2010 - Apr 2022',
       color='Neighborhood') + guides(color = guide_legend(nrow = 8)) +
  theme(legend.justification = c('left', 'top'),
        legend.key.size = unit(0.5, "cm"),
        legend.position = 'top')


ggplot(data = subset(se_med_rent, 
                     se_med_rent$Borough=='Brooklyn' & 
                       se_med_rent$bedroom == 2), 
       aes(x=year, y=med_rent, col=areaName))+ 
  geom_line(aes(group=areaName)) + theme_clean() +
  stat_summary(
    data = ~ subset(se_med_rent, 
                    se_med_rent$Borough=='Brooklyn' & 
                      se_med_rent$bedroom == 2),
    aes(group = -1), col = 'black',
    fun.data = mean_se,
    geom = "line", size = 2) + 
  scale_color_manual(values=as.vector(c(cols25(), kelly()))) +
  labs(x='Month Year', y='Median Rant', 
       title = 
         'Median Rent of 2 Bedroom Apartments in Brooklyn Neighborhoods from Jan 2010 - Apr 2022',
       color='Neighborhood') + guides(color = guide_legend(nrow = 8)) +
  theme(legend.justification = c('left', 'top'),
        legend.key.size = unit(0.5, "cm"),
        legend.position = 'top')

#bronx
ggplot(data = subset(se_med_rent, 
                     se_med_rent$Borough=='Bronx' & 
                       se_med_rent$bedroom == 1), 
       aes(x=year, y=med_rent, col=areaName))+ 
  geom_line(aes(group=areaName)) + theme_clean() +
  stat_summary(
    data = ~ subset(se_med_rent, 
                    se_med_rent$Borough=='Bronx' & 
                      se_med_rent$bedroom == 1),
    aes(group = -1), col = 'black',
    fun.data = mean_se,
    geom = "line", size = 2) + 
  scale_color_manual(values=as.vector(c(cols25(), kelly()))) +
  labs(x='Month Year', y='Median Rant', 
       title = 
         'Median Rent of 1 Bedroom Apartments in Bronx Neighborhoods from Jan 2010 - Apr 2022',
       color='Neighborhood') + guides(color = guide_legend(nrow = 4)) +
  theme(legend.justification = c('left', 'top'),
        legend.key.size = unit(0.5, "cm"),
        legend.position = 'top')


ggplot(data = subset(se_med_rent, 
                     se_med_rent$Borough=='Bronx' & 
                       se_med_rent$bedroom == 2), 
       aes(x=year, y=med_rent, col=areaName))+ 
  geom_line(aes(group=areaName)) + theme_clean() +
  stat_summary(
    data = ~ subset(se_med_rent, 
                    se_med_rent$Borough=='Bronx' & 
                      se_med_rent$bedroom == 2),
    aes(group = -1), 
    fun.data = mean_se, col='black',
    geom = "line", size = 2) + 
  scale_color_manual(values=as.vector(c(cols25(), kelly()))) +
  labs(x='Month Year', y='Median Rant', 
       title = 
         'Median Rent of 2 Bedroom Apartments in Bronx Neighborhoods from Jan 2010 - Apr 2022',
       color='Neighborhood') + guides(color = guide_legend(nrow = 4)) +
  theme(legend.justification = c('left', 'top'),
        legend.key.size = unit(0.5, "cm"),
        legend.position = 'top')


airbnb_subset_long_term  <- subset(airbnb_subset, 
                                  airbnb_subset$maximum_nights >= 182&
                                    airbnb_subset$availability_365 >= 182 & 
                                    airbnb_subset$beds <= 
                                    airbnb_subset$bedrooms &
                                    airbnb_subset$accommodates <= 
                                    2 * airbnb_subset$beds)


ggplot(data = 
         subset(airbnb_subset_long_term, 
                airbnb_subset_long_term$neighbourhood_group_cleansed == 
                  'Manhattan'),
       aes(x=reorder(neighbourhood_cleansed, price, FUN=median), 
           y = price * 28)) +
  geom_boxplot() + facet_wrap(vars(neighbourhood_group_cleansed), 
                              scales = "free") + 
  theme(axis.text.x = element_text(angle = 90)) + coord_flip() + 
  theme_clean() +
  labs(x='Neighborhood', y='Monthly Cost', 
       title = 
         'Boxplot of Airbnb Monthly Cost\nby Neighborhood in March 2022')

ggplot(data = 
         subset(airbnb_subset_long_term, 
                airbnb_subset_long_term$neighbourhood_group_cleansed == 
                  'Manhattan' & 
                  airbnb_subset_long_term$neighbourhood_cleansed != 
                  'Flatiron District'),
       aes(x=reorder(neighbourhood_cleansed, price, FUN=median), 
           y = price * 28)) +
  geom_boxplot(outlier.shape = NA) + 
  facet_wrap(vars(neighbourhood_group_cleansed), 
             scales = "free") + 
  theme(axis.text.x = element_text(angle = 90)) + coord_flip(ylim = c(0, 20000)) + 
  theme_clean() +
  labs(x='Neighborhood', y='Monthly Cost', 
       title = 
         'Boxplot of Airbnb Monthly Cost\nby Neighborhood in March 2022')
       