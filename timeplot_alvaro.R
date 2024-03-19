library(sf)
library(tidyverse)
library(ggplot2)


#Read in the shapefile as an "sf" object
#shapefile: contain special data needed to plot maps
#read_sf: will read simple features, including shapefile
#dsn: data source name
#layer: combination of data, stat and geometry with a potential position adjustment
#shapefile = read_sf(dsn ="base_files", layer= "tl_2019_51_bg")

#read.csv: will read a certain file, data input
health_POIs = read.csv('AllPOIs_Montgomery_VA.csv')

#Read in the time series data
time_data = read.csv("allvisits_VA_new.csv")
#remove records where nobody visited the corresponding healthcare facility
#subset: will indicate which rows to keep
#is.na: not available (since ! is before it would be available)
time_data = subset(time_data, !is.na(time_data$visitor_home_cbg))
#merge the dataset with the locations & types of the healthcare facilities
time_data_merged = merge(time_data,health_POIs,
by.x="safegraph_place",by.y="safegraph_place_id")


#You'll want to make two plots, one with every NAICS code, and then one with the "grouped" NAICS codes, which we will create below:

#Aggregate by NAICS code; this means we will sum up all the different doctors for each NAICS code to get one result per NAICS code
# $ in between is the data set's specified field.
# FUN: function will be a sum.
#Aggregate: get the summary stats of the data group
NAICS_aggregate = aggregate(time_data_merged$number , 
                            by=list(time_data_merged$date,time_data_merged$naics_code), FUN = sum)

# these declare the names of each data set into vectors
names(NAICS_aggregate) = c("date","NAICS","num")

#Here, we will normalize the values per NAICS code. 
#If we tried to plot them without doing this, we would have some numbers way greater than others 

#For example, the average number of people visiting NAICS 621111 (physicians) is 1-2K, 
#while the average number visiting 621340 (physical/occupational/speech therapists) is more like 100

#If we don't normalize, we won't be able to see the therapists numbers because the physician numbers are so high.

#The way we are normalizing is by dividing each number by the median number of trips to that NAICS code; therefore the output number can be represented as "% of trips compared to median"
#2 means 2x as many trips as median, .5 means half as many, etc.
unique_codes = unique(NAICS_aggregate$NAICS)
NAICS_data = data.frame()
for (code in unique_codes){
  sub_data = subset(NAICS_aggregate, NAICS == code)
  sub_data$num_normalized = sub_data$num / median(sub_data$num)
  NAICS_data = rbind(NAICS_data, sub_data)
}

#Plotting
NAICS_data$date = as.Date(NAICS_data$date)

NAICS_data$NAICS = as.factor(NAICS_data$NAICS)

ggplot() + geom_line(data=NAICS_data, mapping = aes(x=date,
                                                    y = num_normalized, colour = NAICS , group = NAICS)) + scale_colour_brewer(palette="Set1")



#plot all data
ggplot(data = NAICS_data, aes(x = date, y = num_normalized, color = NAICS, group = NAICS)) +
  geom_line() +
  labs(title = "Visits to Healthcare Facilities by NAICS Code",
       x = "Date",
       y = "Number of Visits",
       color = "NAICS Code") +
  theme_minimal()

# Plot aggregated data
NAICS_data$agg_group = substr(NAICS_data$NAICS,1,2)
aggregated_data <- NAICS_data %>%
  group_by(date,agg_group) %>%
  summarise(total_num_normalized = mean(num_normalized))

ggplot(data = aggregated_data, aes(x = date, y = total_num_normalized, color = agg_group, group = agg_group)) +
  geom_line() +
  labs(title = "Average Visits to Healthcare Facilities",
       x = "Date",
       y = "Average Number of Visits") +
  theme_minimal()


aggregated_data_wide = pivot_wider(aggregated_data,names_from = agg_group,values_from=total_num_normalized)
aggregated_data_wide$date = as.Date(aggregated_data_wide$date)
names(aggregated_data_wide) = c("date","grocery","department","schools","healthcare","naturepark","restaurants")
cor(aggregated_data_wide$healthcare, aggregated_data_wide$naturepark)
#### March 13 - compare various groups


# nature parks vs healthcare
cor(aggregated_data_wide$naturepark, aggregated_data_wide$healthcare)
# nature parks vs grocery stores
cor(aggregated_data_wide$naturepark, aggregated_data_wide$grocery)
# healthcare vs grocery stores
cor(aggregated_data_wide$healthcare, aggregated_data_wide$grocery)
# department vs schools
cor(aggregated_data_wide$department, aggregated_data_wide$schools)
# grocery vs restaurants
cor(aggregated_data_wide$grocery, aggregated_data_wide$restaurants)
# schools vs healthcare
cor(aggregated_data_wide$schools, aggregated_data_wide$healthcare)


#next, divide into pre-pandemic and pandemic

#nature parks vs healthcare
aggregated_data_wide_prepandemic = subset(aggregated_data_wide, date < as.Date("2020-01-01"))
aggregated_data_wide_pandemic = subset(aggregated_data_wide, date > as.Date("2020-01-01"))
  ##prepandemic
ggplot() + geom_point(data = aggregated_data_wide_prepandemic, mapping = aes(x =naturepark, y = healthcare))+ ggtitle("Prepandemic: Natureparks vs. Healthcare")
cor(aggregated_data_wide_prepandemic$naturepark, aggregated_data_wide_prepandemic$healthcare)
  ##pandemic
ggplot() + geom_point(data = aggregated_data_wide_pandemic, mapping = aes(x =naturepark, y = healthcare))+ ggtitle("Pandemic: Natureparks vs. Healthcare")
cor(aggregated_data_wide_pandemic$naturepark, aggregated_data_wide_pandemic$healthcare)

#nature parks vs grocery stores
  ##prepandemic
ggplot() + geom_point(data = aggregated_data_wide_prepandemic, mapping = aes(x =naturepark, y = grocery))+ ggtitle("Prepandemic: Natureparks vs. Grocery")
cor(aggregated_data_wide_prepandemic$naturepark, aggregated_data_wide_prepandemic$grocery)
  ##pandemic
ggplot() + geom_point(data = aggregated_data_wide_pandemic, mapping = aes(x =naturepark, y = grocery))+ ggtitle("Pandemic: Natureparks vs. Grocery")
cor(aggregated_data_wide_pandemic$naturepark, aggregated_data_wide_pandemic$grocery)

#healthcare vs grocery
  ##prepandemic
ggplot() + geom_point(data = aggregated_data_wide_prepandemic, mapping = aes(x = healthcare, y = grocery))+ ggtitle("Prepandemic: Healthcare vs Grocery")
cor(aggregated_data_wide_prepandemic$healthcare, aggregated_data_wide_prepandemic$grocery)
  ##pandemic
ggplot() + geom_point(data = aggregated_data_wide_pandemic, mapping = aes(x = healthcare, y = grocery))+ ggtitle("Pandemic: Healthcare vs. Grocery")
cor(aggregated_data_wide_pandemic$healthcare, aggregated_data_wide_pandemic$grocery)

#departments vs schools
  ##prepandemic
ggplot() + geom_point(data = aggregated_data_wide_prepandemic, mapping = aes(x = department, y = schools))+ ggtitle("Prepandemic: Department vs. Schools")
cor(aggregated_data_wide_prepandemic$department, aggregated_data_wide_prepandemic$schools)
  ##pandemic
ggplot() + geom_point(data = aggregated_data_wide_pandemic, mapping = aes(x = department, y = schools))+ ggtitle("Pandemic: Department vs. Schools")
cor(aggregated_data_wide_pandemic$department, aggregated_data_wide_pandemic$schools)

#grocery vs restaurants

  ##prepandemic
ggplot() + geom_point(data = aggregated_data_wide_prepandemic, mapping = aes(x = restaurants, y = grocery))+ ggtitle("Prepandemic: Restaurants vs. Grocery")
cor(aggregated_data_wide_prepandemic$restaurants, aggregated_data_wide_prepandemic$grocery)
  ##pandemic
ggplot() + geom_point(data = aggregated_data_wide_pandemic, mapping = aes(x = restaurants, y = grocery))+ ggtitle("Pandemic: Restaurants vs. Grocery")
cor(aggregated_data_wide_pandemic$restaurants, aggregated_data_wide_pandemic$grocery)

#schools vs healthcare
  ##prepandemic
ggplot() + geom_point(data = aggregated_data_wide_prepandemic, mapping = aes(x = schools, y = healthcare))+ ggtitle("Prepandemic: Schools vs. Healthcare")
cor(aggregated_data_wide_prepandemic$schools, aggregated_data_wide_prepandemic$healthcare)
  ##pandemic
ggplot() + geom_point(data = aggregated_data_wide_pandemic, mapping = aes(x = schools, y = healthcare))+ ggtitle("Pandemic: Schools vs. Healthcare")
cor(aggregated_data_wide_pandemic$schools, aggregated_data_wide_pandemic$healthcare)



# Plot mean data
mean_data <- NAICS_data %>%
  group_by(date) %>%
  summarise(total_num_normalized = mean(num_normalized))

ggplot(data = mean_data, aes(x = date, y = total_num_normalized)) +
  geom_line() +
  labs(title = "Average Visits to Healthcare Facilities",
       x = "Date",
       y = "Average Number of Visits") +
  theme_minimal()



