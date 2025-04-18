# set working directory first - ALWAYS 2, 3, and 4 load packages from library
library(sf)
library(tidyverse)
library(ggplot2)


#Read in the shapefile as an "sf" (geographic data) object 
#shapefile = read_sf(dsn ="base_files", layer= "tl_2019_51_bg")
health_POIs = read.csv('HealthPOIs_Montgomery_VA 2.csv')

#Read in the time series data - reads in file from the viewer
time_data = read.csv("healthcarevisits_VA_new.csv")
#remove records where nobody visited the corresponding healthcare facility - removes n/a from list of places
time_data = subset(time_data, !is.na(time_data$visitor_home_cbg))
#merge the dataset with the locations & types of the healthcare facilities - combining the two sets of data together
time_data_merged = merge(time_data,health_POIs,by.x="safegraph_place",by.y="safegraph_place_id")


#Aggregate by NAICS code; this means we will sum up all the different doctors for each NAICS code to get one result per NAICS code - sums the totals for NAICS
NAICS_aggregate = aggregate(time_data_merged$number , by=list(time_data_merged$date,time_data_merged$city), FUN = sum)
names(NAICS_aggregate) = c("date","city","num")

#Here, we will normalize the values per NAICS code. If we tried to plot them without doing this, we would have some numbers way greater than others 
#For example, the average number of people visiting NAICS 621111 (physicians) is 1-2K, while the average number visiting 621340 (physical/occupational/speech therapists) is more like 100
#If we don't normalize, we won't be able to see the therapists numbers because the physician numbers are so high.

#The way we are normalizing is by dividing each number by the median number of trips to that NAICS code; therefore the output number can be represented as "% of trips compared to median"
#2 means 2x as many trips as median, .5 means half as many, etc. - this changes the count to % and this is done for each NAICS
unique_codes = unique(NAICS_aggregate$city)
NAICS_data = data.frame()
for (code in unique_codes){
  sub_data = subset(NAICS_aggregate, city == code)
  sub_data$num_normalized = sub_data$num / median(sub_data$num)
  NAICS_data = rbind(NAICS_data, sub_data)
}

#Plotting
NAICS_data$date = as.Date(NAICS_data$date)
NAICS_data$NAICS = as.factor(NAICS_data$city)
ggplot() + geom_line(data=NAICS_data, mapping = aes(x=date, y = num_normalized, colour = city , group = city)) + scale_colour_brewer(palette="Set1")


#### Another option is you could split up between nova and swva
#### To do this, we'll create a new variable 

#### NOVA cities: Leesburg, Lansdowne
#### SWVA cities: Blacksburg, Merrimac, Dublin, Christiansburg, Radford, Fairlawn, Shawsville
NOVA<-c("Leesburg","Lansdowne")
SWVA<-c("Blacksburg", "Merrimac", "Dublin", "Christiansburg", "Radford", "Fairlawn", "Shawsville")

time_data_merged$region = ""
is.element(time_data_merged$city, NOVA)

time_data_merged$city[is.element(time_data_merged$city, NOVA)]

time_data_merged$region[is.element(time_data_merged$city, NOVA)] = "NOVA"
time_data_merged$region[is.element(time_data_merged$city, SWVA)] = "SWVA"
table(time_data_merged$region)

# 2/12: We need to aggregate so that we sum up all trips to any doctor on each date, for SWVA and NOVA
# aggregate works by aggregating the first argument, then a list of aggregating variables in a "by" argument, then a function that specifies how the aggregation should be done
NAICS_aggregate = aggregate(time_data_merged$number, by = list(time_data_merged$region, time_data_merged$date), FUN = sum)
names(NAICS_aggregate) = c("region","date","number")

#turn this into region based
NAICS_data = data.frame()
# 
# for (code in unique(NAICS_aggregate$region)){
#   print(code)
#   sub_data = subset(NAICS_aggregate, region == code)
#   sub_data$num_normalized = sub_data$num / median(sub_data$num)
#   NAICS_data = rbind(NAICS_data, sub_data)
# }
for (code in c("NOVA","SWVA")){
  sub_data = subset(NAICS_aggregate, region == code)
  sub_data$num_normalized = sub_data$num / median(sub_data$num)
  NAICS_data = rbind(NAICS_data, sub_data)
}

#Plotting
NAICS_data$date = as.Date(NAICS_data$date)
NAICS_data$NAICS = as.factor(NAICS_data$region)
ggplot() + geom_line(data=NAICS_data, mapping = aes(x=date, y = num_normalized, colour = region , group = region)) + scale_colour_brewer(palette="Set1")

