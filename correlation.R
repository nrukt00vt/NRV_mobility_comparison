library(sf)
library(tidyverse)
library(ggplot2)


#read.csv: will read a certain file, data input
health_POIs = read.csv('AllPOIs_Montgomery_VA.csv')

#Read in the time series data
time_data = read.csv("allvisits_VA_new.csv")
time_data = subset(time_data, !is.na(time_data$visitor_home_cbg))
#merge the dataset with the locations & types of the healthcare facilities
time_data_merged = merge(time_data,health_POIs,
by.x="safegraph_place",by.y="safegraph_place_id")

time_data_merged=subset(time_data_merged, !is.element(city,c("Leesburg","Lansdowne")))

#Aggregate by NAICS code
NAICS_aggregate = aggregate(time_data_merged$number , 
                            by=list(time_data_merged$date,time_data_merged$naics_code), FUN = sum)

names(NAICS_aggregate) = c("date","NAICS","num")

#Normalize the values per NAICS code. 
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

aggregated_data <- aggregate(NAICS_data$num_normalized, FUN = mean, by = list(NAICS_data$agg_group,
                                                                              NAICS_data$date))
names(aggregated_data) = c("agg_group", "date","total_num_normalized")
ggplot(data = aggregated_data, aes(x = date, y = total_num_normalized, color = agg_group, group = agg_group)) +
  geom_line() +
  labs(title = "Average Visits to Healthcare Facilities",
       x = "Date",
       y = "Average Number of Visits") +
  theme_minimal()

ggplot(data = subset(aggregated_data,agg_group == 62), aes(x = date, y = total_num_normalized, color = agg_group, group = agg_group)) +
  geom_line() +
  labs(title = "Average Visits to Healthcare Facilities",
       x = "Date",
       y = "Average Number of Visits") +
  theme_minimal()


#normalizing by date; accounting for the fact that total trips changed over time and we want to see how proportionally the number of trips to different places correlated over time
aggregated_data$date_num = 0
for (i in 1:nrow(aggregated_data)){
  aggregated_data$date_num[i] = mean(subset(aggregated_data,date == aggregated_data$date[i])$total_num_normalized)
}
aggregated_data$normalized_by_date = aggregated_data$total_num_normalized/aggregated_data$date_num
aggregated_data_2 = aggregated_data[,c("date", "agg_group","normalized_by_date")]
aggregated_data_wide = pivot_wider(aggregated_data,names_from = agg_group,values_from=total_num_normalized)
aggregated_data$date_num = 0
for (i in 1:nrow(aggregated_data)){
  aggregated_data$date_num[i] = mean(subset(aggregated_data,date == aggregated_data$date[i])$total_num_normalized)
}
aggregated_data$normalized_by_date = aggregated_data$total_num_normalized/aggregated_data$date_num
aggregated_data_2 = aggregated_data[,c("date", "agg_group","normalized_by_date")]
aggregated_data_wide = pivot_wider(aggregated_data_2,names_from = agg_group,values_from=normalized_by_date)
aggregated_data_wide$date = as.Date(aggregated_data_wide$date)
names(aggregated_data_wide) = c("date","Grocery","Department stores","Schools","Healthcare","Nature parks","Restaurants")

#Pre-initial lockdowns in the USA
aggregated_wide_pre = subset(aggregated_data_wide, date < "2020-03-15")

test_cor_pre = as.data.frame(subset(aggregated_wide_pre , select = -c(date)))
corplot_pre = cor(test_cor_pre, method = c("spearman"))

library(ggcorrplot)
ggcorrplot(corplot_pre, hc.order = TRUE, p.mat=corplot_pre_ps, colors = c("#d95f02","#f0f0f0","#1b9e77"), ggtheme = ggplot2::theme_minimal, show.legend = F)

#after the start of lockdowns
aggregated_wide_post = subset(aggregated_data_wide, date > "2020-03-15")

test_cor_post = as.data.frame(subset(aggregated_wide_post , select = -c(date)))
test_cor_post = test_cor_post[,c("Nature parks","Schools","Restaurants","Department stores","Grocery","Healthcare")]
corplot_post = cor(test_cor_post, method = c("spearman"))
corplot_post_ps = cor_pmat(test_cor_post, method = "spearman")
corrplot(as.matrix(corplot_post))

ggcorrplot(corplot_post, p.mat=corplot_post_ps, colors = c("#d95f02","#f0f0f0","#1b9e77"))

### Google data 
google_data = read.csv("~/Downloads/mobility_report_US.csv")

google_data_national = subset(google_data, state == "Total")

google_data_national = subset(google_data_national, select = -c(state,county))
google_data_national_pre = subset(google_data_national, date > as.Date("2021-08-01"))

google_data_national_pre_long= pivot_longer(google_data_national_pre, cols = c(retail.and.recreation, grocery.and.pharmacy, parks, transit.stations,workplaces,residential))



#same normalization process
google_data_national_pre_long$date_num = 0
for (i in 1:nrow(google_data_national_pre_long)){
  google_data_national_pre_long$date_num[i] = mean(subset(google_data_national_pre_long,date == google_data_national_pre_long$date[i])$value)
}
google_data_national_pre_long$normalized_by_date = google_data_national_pre_long$value-google_data_national_pre_long$date_num
google_data_national_pre_long_2 = google_data_national_pre_long[,c("date", "name","normalized_by_date")]
google_data_national_pre_long_2_wide = pivot_wider(google_data_national_pre_long_2,names_from = name,values_from=normalized_by_date)
google_data_national_pre_long_2_wide$date = as.Date(google_data_national_pre_long_2_wide$date)

test_cor_pre = as.data.frame(subset(google_data_national_pre_long_2_wide , select = -c(date)))
corplot_pre = cor(test_cor_pre, method = c("spearman"))
library(corrplot)
corrplot(as.matrix(corplot_pre),method = "color", type="lower")

