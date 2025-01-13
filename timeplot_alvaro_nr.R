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

time_data_merged=subset(time_data_merged, !is.element(city,c("Leesburg","Lansdowne")))
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

ggplot(data = subset(aggregated_data,agg_group == 62), aes(x = date, y = total_num_normalized, color = agg_group, group = agg_group)) +
  geom_line() +
  labs(title = "Average Visits to Healthcare Facilities",
       x = "Date",
       y = "Average Number of Visits") +
  theme_minimal()

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
names(aggregated_data_wide) = c("date","grocery","department","schools","healthcare","naturepark","restaurants")
#### March 13 - compare various groups
aggregated_wide_pre = subset(aggregated_data_wide, date < "2020-03-15")

test_cor_pre = as.data.frame(subset(aggregated_wide_pre , select = -c(date)))
corplot_pre = cor(test_cor_pre, method = c("spearman"))
corrplot(as.matrix(corplot_pre))

aggregated_wide_post = subset(aggregated_data_wide, date > "2020-03-15")

test_cor_post = as.data.frame(subset(aggregated_wide_post , select = -c(date)))
corplot_post = cor(test_cor_post, method = c("spearman"))
corrplot(as.matrix(corplot_post))

#correlation
# nature parks vs healthcare
aggregated_data_wide_pre = subset(aggregated_data_wide, date < "2020-03-15")


cor(aggregated_data_wide_pre$naturepark, aggregated_data_wide_pre$healthcare)

ggplot()+ geom_point(data = aggregated_data_wide_pre, mapping = 
                       aes(x =naturepark, y = healthcare, colour=date))+
  scale_colour_date(low = "green", high = "#F11B00")+
  ggtitle("Prepandemic: Natureparks vs. Healthcare")
# nature parks vs grocery stores
cor(aggregated_data_wide_pre$naturepark, aggregated_data_wide_pre$grocery)
# healthcare vs grocery stores
cor(aggregated_data_wide_pre$healthcare, aggregated_data_wide_pre$grocery)
# department vs schools
cor(aggregated_data_wide_pre$department, aggregated_data_wide_pre$schools)
# grocery vs restaurants
cor(aggregated_data_wide_pre$grocery, aggregated_data_wide_pre$restaurants)
# schools vs healthcare
cor(aggregated_data_wide_pre$schools, aggregated_data_wide_pre$healthcare)

# nature parks vs healthcare
aggregated_data_wide_post = subset(aggregated_data_wide, date > "2020-03-15")


cor(aggregated_data_wide_post$naturepark, aggregated_data_wide_post$healthcare)

ggplot()+ geom_point(data = aggregated_data_wide_post, mapping = 
                       aes(x =naturepark, y = healthcare, colour=date))+
  scale_colour_date(low = "green", high = "#F11B00")+
  ggtitle("postpandemic: Natureparks vs. Healthcare")
# nature parks vs grocery stores
cor(aggregated_data_wide_post$naturepark, aggregated_data_wide_post$grocery)
# healthcare vs grocery stores
cor(aggregated_data_wide_post$healthcare, aggregated_data_wide_post$grocery)
# department vs schools
cor(aggregated_data_wide_post$department, aggregated_data_wide_post$schools)
# grocery vs restaurants
cor(aggregated_data_wide_post$grocery, aggregated_data_wide_post$restaurants)
# schools vs healthcare
cor(aggregated_data_wide_post$schools, aggregated_data_wide_post$healthcare)


#next, divide into pre-pandemic and pandemic


### Should we look at the correlation with cases?
casedata = read.csv("~/Downloads/dalityandweeklycasedatabylocality.csv")

library(EpiEstim)
casedata = read.csv("VDH Case Data/dalityandweeklycasedatabylocality.csv")

library(chron)

casedata = casedata[-which(is.na(casedata$newcases_weekly)),]

alldata = data.frame()
counties = unique(casedata$locality)
for (i in 1:length(counties)){
  casedata_test = subset(casedata, locality == counties[i])
  
  casedata_test$dates = as.Date(casedata_test$reportdate,format = "%d%b%Y")
  casedata_dates = data.frame(dates = casedata_test$dates,I = casedata_test$newcases_weekly)
  casedata_dates = casedata_dates[order(casedata_dates$dates),]
  casedata_dates$I[which(casedata_dates$I<0)] = 0
  casedata_dates$dates = as.Date(casedata_dates$dates,format = "%d%b%Y")
  res_parametric_si <- estimate_R(casedata_dates, 
                                  method="parametric_si",
                                  config = make_config(list(
                                    mean_si = 7, 
                                    std_si = 1.5))
  )
  casedata_test$R = 0
  outputr = res_parametric_si$R
  outputr$t = (outputr$t_start+outputr$t_end)/2
  outputr$date = outputr$t + min(casedata_test$dates)
  casedata_test= merge(casedata_test,outputr,by.x="dates", by.y = "date")
  alldata=rbind(alldata,casedata_test)
  print(i)
}
alldata = subset(alldata, is.element(vdhhealthdistrict,c("New River")))
alldata2 = subset(alldata, select = c(dates,`Median(R)`))
alldata2 = aggregate(alldata2$`Median(R)`,by=list(alldata2$dates), FUN = mean)
names(alldata2) = c("date", "R_val")
aggregated_data_wide = merge(aggregated_data_wide,alldata2)

test_cor = as.data.frame(subset(aggregated_data_wide , select = -c(date)))
corplot = cor(test_cor, method = c("spearman"))
library(corrplot)
corrplot(as.matrix(corplot))


aggregated_wide_pre = subset(aggregated_data_wide, date < "2019-06-01")
test_cor_pre = as.data.frame(subset(aggregated_wide_pre , select = -c(date)))
corplot_pre = cor(test_cor_pre, method = c("spearman"))
corrplot(as.matrix(corplot_pre))
aggregated_wide_pre = subset(aggregated_data_wide, date < "2020-03-15")
test_cor_pre = as.data.frame(subset(aggregated_wide_pre , select = -c(date)))
corplot_pre = cor(test_cor_pre, method = c("spearman"))
corrplot(as.matrix(corplot_pre))
aggregated_wide_during = subset(aggregated_data_wide, date > "2020-03-15")
test_cor_during = as.data.frame(subset(aggregated_wide_during , select = -c(date)))
corplot_during = cor(test_cor_during, method = c("spearman"))
corrplot(as.matrix(corplot_during))


aggregated_wide_post = subset(aggregated_data_wide, date > "2020-06-15")
aggregated_wide_post=subset(aggregated_wide_post, R_val < 5)
test_cor_post = as.data.frame(subset(aggregated_wide_post , select = -c(date)))
corplot_post = cor(test_cor_post, method = c("spearman"))
corrplot(as.matrix(corplot_post))

#Negative correlation in trips to 


aggregated_wide_post = subset(aggregated_data_wide, date < "2019-12-30" & date > "2019-06-01")
test_cor_post = as.data.frame(subset(aggregated_wide_post , select = -c(date)))
corplot_post = cor(test_cor_post, method = c("spearman"))
corrplot(as.matrix(corplot_post))

ggplot() + geom_point(data = aggregated_wide_post, mapping = 
                        aes(x =naturepark, y = department, colour=date))+ 
  scale_colour_date(low = "green", high = "#F11B00")+
  ggtitle("Pandemic: Natureparks vs. Healthcare")

ggplot() + geom_point(data = aggregated_wide_post, mapping = 
                        aes(x =healthcare, y = R_val, colour=date))+ 
  scale_colour_date(low = "green", high = "#F11B00")+
  ggtitle("Pandemic: Natureparks vs. Healthcare")
## Identifiably negative trend between nature parks and healthcare
cor(aggregated_data_wide_pandemic$naturepark, aggregated_data_wide_pandemic$healthcare)
      #nature parks vs healthcare
aggregated_data_wide_prepandemic = subset(aggregated_data_wide, date < as.Date("2020-03-15"))
aggregated_data_wide_pandemic = subset(aggregated_data_wide, date > as.Date("2020-03-15"))
  ##prepandemic
ggplot()+ geom_point(data = aggregated_data_wide_prepandemic, mapping = 
                        aes(x =naturepark, y = healthcare, colour=date))+
  scale_colour_date(low = "green", high = "#F11B00")+
  ggtitle("Prepandemic: Natureparks vs. Healthcare")
cor(aggregated_data_wide_prepandemic$naturepark, aggregated_data_wide_prepandemic$healthcare)
  ##pandemic
ggplot() + geom_point(data = aggregated_data_wide_pandemic, mapping = 
                        aes(x =naturepark, y = healthcare, colour=date))+ 
  scale_colour_date(low = "green", high = "#F11B00")+
  ggtitle("Pandemic: Natureparks vs. Healthcare")
cor(aggregated_data_wide_pandemic$naturepark, aggregated_data_wide_pandemic$healthcare)

#nature parks vs grocery stores
  ##prepandemic
ggplot() + geom_point(data = aggregated_data_wide_prepandemic, mapping = 
                        aes(x =naturepark, y = grocery))+ ggtitle("Prepandemic: Natureparks vs. Grocery")
cor(aggregated_data_wide_prepandemic$naturepark, aggregated_data_wide_prepandemic$grocery)
  ##pandemic
ggplot() + geom_point(data = aggregated_data_wide_pandemic, mapping = 
                        aes(x =naturepark, y = grocery))+ ggtitle("Pandemic: Natureparks vs. Grocery")
cor(aggregated_data_wide_pandemic$naturepark, aggregated_data_wide_pandemic$grocery)

#healthcare vs grocery
  ##prepandemic
ggplot() + geom_point(data = aggregated_data_wide_prepandemic, mapping = 
                        aes(x = healthcare, y = grocery))+ ggtitle("Prepandemic: Healthcare vs Grocery")
cor(aggregated_data_wide_prepandemic$healthcare, aggregated_data_wide_prepandemic$grocery)
  ##pandemic
ggplot() + geom_point(data = aggregated_data_wide_pandemic, mapping = 
                        aes(x = healthcare, y = grocery))+ ggtitle("Pandemic: Healthcare vs. Grocery")
cor(aggregated_data_wide_pandemic$healthcare, aggregated_data_wide_pandemic$grocery)

#departments vs schools
  ##prepandemic
ggplot() + geom_point(data = aggregated_data_wide_prepandemic, mapping = 
                        aes(x = department, y = schools))+ ggtitle("Prepandemic: Department vs. Schools")
cor(aggregated_data_wide_prepandemic$department, aggregated_data_wide_prepandemic$schools)
  ##pandemic
ggplot() + geom_point(data = aggregated_data_wide_pandemic, mapping = 
                        aes(x = department, y = schools))+ ggtitle("Pandemic: Department vs. Schools")
cor(aggregated_data_wide_pandemic$department, aggregated_data_wide_pandemic$schools)

#grocery vs restaurants

  ##prepandemic
ggplot() + geom_point(data = aggregated_data_wide_prepandemic, mapping = 
                        aes(x = restaurants, y = grocery))+ ggtitle("Prepandemic: Restaurants vs. Grocery")
cor(aggregated_data_wide_prepandemic$restaurants, aggregated_data_wide_prepandemic$grocery)
  ##pandemic
ggplot() + geom_point(data = aggregated_data_wide_pandemic, mapping = 
                        aes(x = restaurants, y = grocery))+ ggtitle("Pandemic: Restaurants vs. Grocery")
cor(aggregated_data_wide_pandemic$restaurants, aggregated_data_wide_pandemic$grocery)

#schools vs healthcare
  ##prepandemic
ggplot() + geom_point(data = aggregated_data_wide_prepandemic, mapping = 
                        aes(x = schools, y = healthcare))+ ggtitle("Prepandemic: Schools vs. Healthcare")
cor(aggregated_data_wide_prepandemic$schools, aggregated_data_wide_prepandemic$healthcare)
  ##pandemic
ggplot() + geom_point(data = aggregated_data_wide_pandemic, mapping = 
                        aes(x = schools, y = healthcare))+ ggtitle("Pandemic: Schools vs. Healthcare")
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



