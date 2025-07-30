
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



#Aggregate by NAICS code; this means we will sum up all the different doctors for each NAICS code to get one result per NAICS code
NAICS_aggregate = aggregate(time_data_merged$number , by=list(time_data_merged$date,time_data_merged$safegraph_place, time_data_merged$naics_code), FUN = sum)
names(NAICS_aggregate) = c("date","location","naics","num")

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
NAICS_data$naics_sub = substr(NAICS_data$naics,1,2)

agg_NAICS = aggregate(NAICS_data$num, by =list(NAICS_data$location), FUN = median )
names(agg_NAICS) = c("location","num_median")

agg_NAICS$cat_num = cut(agg_NAICS$num_median, breaks = c(0,5,50, Inf))
agg_NAICS = merge(agg_NAICS, health_POIs, by.x = "location", by.y = "safegraph_place_id")
uniquelocs = unique(NAICS_data[c("location","naics_sub")])
uniquelocs = merge(uniquelocs,agg_NAICS,by="location")
uniquelocs_NRV=subset(uniquelocs, !is.element(city,c("Leesburg","Lansdowne")))
uniquelocs_NOVA=subset(uniquelocs, is.element(city,c("Leesburg","Lansdowne")))




library(dplyr)
testout_NRV = uniquelocs %>% group_by(naics_sub,cat_num) %>% slice_sample(n=2)
testout_NOVA = uniquelocs %>% group_by(naics_sub,cat_num) %>% slice_sample(n=1)


testout2 = rbind(testout_NRV,testout_NOVA)

testpts = st_as_sf(testout2, coords = c("longitude","latitude"))


testpts= st_set_crs(testpts,st_crs(shapefile))
test_points = testpts

NAICS_data_sub = subset(NAICS_data, is.element(location, test_points$location))

#Plotting
NAICS_data_sub$date = as.Date(NAICS_data_sub$date)


ggplot() + geom_line(data=NAICS_data_sub, mapping = aes(x=date,
                                                        y = num_normalized, colour = location , group = location)) +scale_colour_discrete(guide = "none")


subset(NAICS_data_sub, num_normalized>6)$location
subset(health_POIs, safegraph_place_id ==subset(NAICS_data_sub, num_normalized>6)$location)
testpts = subset(testpts, location !=subset(NAICS_data_sub, num_normalized>6)$location)
#not sure if possible with given data, but could think about mapping individuals to see if one person made a trip to a healthcare facility multiple times
#could do research and see if there were any Covid-19 spikes in the NRV area and see if it correlates with any of the spikes or drops in visits during the time period

library(sf)
test_shp = st_read(dsn = "D:/Downloads/VirginiaBuildingFootprint.shp", layer = "VirginiaBuildingFootprint")
shapefile = read_sf(dsn ="base_files", layer= "tl_2019_51_bg")
testpts= st_set_crs(testpts,st_crs(shapefile))
test_points = testpts

st_can_transform(test_shp, test_points)
test_shp = st_transform(test_shp,st_crs(test_points))

test_shp_valid = test_shp[st_is_valid(test_shp)==TRUE,]

out <- st_nearest_feature(test_points, test_shp_valid)
ggplot(test_shp_valid[out,]) + geom_sf()
testpoly = st_zm( test_shp_valid[out,])
testpoly$naics_code = test_points$naics_code
testpoly$location_name = test_points$location_name
write_sf(testpoly, dsn="testshp_final.shp")
st_write(testpoly, "testshp3_final.shp")

NAICS_data_sub = subset(NAICS_data, is.element(location, test_points$location))

#Plotting
NAICS_data_sub$date = as.Date(NAICS_data_sub$date)


ggplot() + geom_line(data=NAICS_data_sub, mapping = aes(x=date,
                                                    y = num_normalized, colour = location , group = location)) +scale_colour_discrete(guide = "none")


