
library(sf)
library(tidyverse)
library(ggplot2)


#Read in the shapefile as an "sf" object
#shapefile = read_sf(dsn ="base_files", layer= "tl_2022_51_bg")
health_POIs = read.csv('AllPOIs_Montgomery_VA.csv')

#Read in the time series data
time_data = read.csv("allvisits_VA_new.csv")
#remove records where nobody visited the corresponding healthcare facility (this removes the facility that says na)
time_data = subset(time_data, !is.na(time_data$visitor_home_cbg))

#merge the dataset with the locations & types of the healthcare facilities
time_data_merged = merge(time_data,health_POIs,by.x="safegraph_place",by.y="safegraph_place_id")


#Subset the data so that you only have the 3 desired locations; below will subset just one as an example
#time_data_merged = subset(time_data_merged, is.element(location_name, c("Loudoun Women's Healthcare Associates","Society of Radiologists & Ultrasound","Gastroenterolgoy & Hepatology Center","Hancock Orthodontics","New River Dermatology","Farhi Vision & Glaucoma Specialists","Aesthetica Cosmetic Surgery & Laser Center")))

#Aggregate by NAICS code; this means we will sum up all the different doctors for each NAICS code to get one result per NAICS code
NAICS_aggregate = aggregate(time_data_merged$number , by=list(time_data_merged$date,time_data_merged$location_name), FUN = sum)
names(NAICS_aggregate) = c("date","location","num")
average_trips = NAICS_aggregate %>% group_by(location) %>% summarise(average = mean(num))
locations_enough_data = average_trips$location[which(average_trips$average > 15)]

health_POIs_enough_data = subset(health_POIs, is.element(location_name,locations_enough_data))
write.csv(health_POIs_enough_data,"health_POIs_with_data.csv")
#Here, we will normalize the values per NAICS code. If we tried to plot them without doing this, we would have some numbers way greater than others 
#For example, the average number of people visiting NAICS 621111 (physicians) is 1-2K, whilethe average number visiting 621340 (physical/occupational/speech therapists) is more like 100
#If we don't normalize, we won't be able to see the therapists numbers because the physician numbers are so high.

#The way we are normalizing is by dividing each number by the median number of trips to that NAICS code; therefore the output number can be represented as "% of trips compared to median"
#2 means 2x as many trips as median, .5 means half as many, etc. (normalizing the numbers to show % of people compared to median instead of raw number of visits)
unique_codes = unique(NAICS_aggregate$location)
NAICS_data = data.frame()
for (code in unique_codes){
  sub_data = subset(NAICS_aggregate, location == code)
  sub_data$num_normalized = sub_data$num / median(sub_data$num)
  NAICS_data = rbind(NAICS_data, sub_data)
}

#this makes things like 622110 NAICS a variable
#Plotting
NAICS_data$date = as.Date(NAICS_data$date)
NAICS_data$location = as.factor(NAICS_data$location)
ggplot() + geom_line(data=NAICS_data, mapping = aes(x=date, y = num_normalized, colour = location , group = location),size = 1) + scale_colour_brewer(palette="Set1")


#not sure if possible with given data, but could think about mapping individuals to see if one person made a trip to a healthcare facility multiple times
#could do research and see if there were any Covid-19 spikes in the NRV area and see if it correlates with any of the spikes or drops in visits during the time period

