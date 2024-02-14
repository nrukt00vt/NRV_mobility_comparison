#Librarys
#########
library(sf)
library(ggplot2)
library(scales)
library(crayon)
library(tidyverse)
library(dplyr)
library(plotly)
library(leaflet)
library(rgdal)
library(RColorBrewer)
library(xts)
library(leaflet.extras)
library(readr)
library(devtools) 
library(rgeos)
library(shiny)
library(xts)
##########################
#POI graphing Data Process
##########################

#Read in the shapefile as an "sf" object
shapefile = read_sf(dsn =".", layer= "tl_2022_51_bg")
#Choose HealthPOIs_Montgomery_VA.csv
health_POIs = read.csv('HealthPOIs_Montgomery_VA 2.csv')
montgomery_health_POIs1 = subset(health_POIs, city == "Blacksburg" | city == "Christiansburg")
montgomery_health_POIs2 <- montgomery_health_POIs1 %>% distinct(street_address, .keep_all = TRUE)


y <- montgomery_health_POIs2$latitude
x <- montgomery_health_POIs2$longitude
business <- montgomery_health_POIs2$location_name
location_code <- montgomery_health_POIs2$safegraph_place_id
coordinates = data.frame(x, y, business, location_code)

#Convert the coordinates to an "sf" object, so that we can overlay
coordinates_sf = st_as_sf(coordinates, coords = c("x","y"))
#set the projection of the new coordinate object
st_crs(coordinates_sf) <- 4269

#Read in data
all_data = read.csv("overall_trips_VA.csv")
locations_with_data = merge(coordinates,all_data, by.x="location_code",by.y="safegraph_place")
#Tell R the "date" variable is actually a date
all_data$date2 = as.Date(all_data$date, format = "%Y-%m-%d")

shapefile$county_fips = paste0(shapefile$STATEFP,shapefile$COUNTYFP)
#FIPS codes within Montgomery county and with census data
montgomery_selection = c("51121020500", "51121020201","51121020100", 
                         "51121020600", "51121020202","51121021000", 
                         "51121020900", "51121021400") 

#Construct baseline values for each census tract
GEOID_baselines = data.frame(GEOID = unique(all_data$GEOID))
GEOID_baselines$baseline = 0

for (i in 1:nrow(GEOID_baselines)){
  print(i)
  GEOID_sub = GEOID_baselines$GEOID[i]
  sub_dat = subset(all_data, GEOID == GEOID_sub & date2 > as.Date("2020-02-01") & date2 > as.Date("2020-03-01"))
  
  GEOID_baselines$baseline[i] = mean(sub_dat$flow_in)
}

#Merge baseline with dataset
all_data_with_baseline = merge(all_data, GEOID_baselines, by = "GEOID")
#calculate the % of movement each day, compared to the baseline for that census tract
all_data_with_baseline$adjusted_flow_in = all_data_with_baseline$flow_in/all_data_with_baseline$baseline
#Remove census tracts with no data
all_data_with_baseline = subset(all_data_with_baseline,!is.na(adjusted_flow_in))

all_data_with_baseline$ct_status = "None"
#Subset out hospitals

all_data_with_baseline$ct_status[is.element(all_data_with_baseline$GEOID, montgomery_selection)] = "Montgomery"

#Create a dataset with the 10% quantile of movement, mean, and 90th for the census tracts marked "Hospital"
#First, we'll subset ONLY data where the census tract has hospitals
all_data_with_baseline_mont = subset(all_data_with_baseline, ct_status == "Montgomery")

###############################
#Leaflet Map of all health POIs
###############################

#creates an icon for our markers
icons <- awesomeIcons(
  icon = "ios-medkit",
  iconColor = 'blue',
  library = 'ion',
  markerColor = "red"
)

#Health POI plot
m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = Montgomery,
              stroke = TRUE,
              weight = 0.5,
              color = "#37B1ED",
              opacity = 1,
              fillColor = "#37B1ED",
              fillOpacity = 0.25,
              popup = ~GEOID,
              highlightOptions = highlightOptions(color = "#E2068A", weight = 1.5,
                                                  bringToFront = TRUE, fillOpacity = 0.5),) %>%
  addAwesomeMarkers(data = coordinates, 
                    ~x, ~y, icon = icons,
                    clusterOptions = markerClusterOptions(),
                    popup = ~business,) %>%
  setView(lng = -80.412,
          lat = 37.187,
          zoom = 10)
m
###############################
#Constructing POI GeoJSON files
###############################
#Setting up overall_trips.csv
overall_trips <- read.csv('overall_trips_VA.csv')
overall_trips$GEOID <- as.character(overall_trips$visitor_home_cbg)
overall_trips$location_name <- 0
overall_trips$latitude <- 0
overall_trips$longitude <- 0
overall_trips$NAICS_code <- 0
print(overall_trips$safegraph_place[1])
print(health_POIs$safegraph_place_id[1])
for (i in 1:length(overall_trips$safegraph_place)){
  for (j in 1:length(health_POIs$safegraph_place_id)){
    if (overall_trips$safegraph_place[i] == health_POIs$safegraph_place_id[j]){
      overall_trips$location_name[i] <- health_POIs$location_name[j]
      overall_trips$latitude[i] <- health_POIs$latitude[j]
      overall_trips$longitude[i] <- health_POIs$longitude[j]
      overall_trips$NAICS_code[i] <- health_POIs$naics_code[j]
    }
  }
}
overall_trips = locations_with_data
overall_trips$NAICS_code <- overall_trips$naics_code
overall_trips$safegraph_place <- overall_trips$safegraph_place_id
uniqueLocations <- overall_trips %>% distinct(safegraph_place, .keep_all = TRUE)
#Dividing out each POI into individual dataframe titled with business name and putting it into a list
#ids = a dataframe containing list of unique POI's
getUniqueSafegraphFrames <- function(ids){
  
  listofids <- list()
  
  for (i in 1:length(ids)){
    sub_dat3 <- subset(overall_trips, safegraph_place == ids[i])
    if (length(sub_dat3$location_name) > 1){
      for (j in 2:length(sub_dat3$location_name)){
        sub_dat3$latitude[j] <- 0
        sub_dat3$longitude[j] <- 0
        sub_dat3$location_name[j] <- 'NULL'
      }
    }else{}
    listofids[[sub_dat3$location_name[1]]] <- sub_dat3
  }
  return(listofids)
  
}
individual_POI_movement <- list()
individual_POI_movement <- getUniqueSafegraphFrames(uniqueLocations$safegraph_place)
#Putting each list into individual map file, 
getUniqueSafegraphFrames(uniqueLocations$safegraph_place)
shapefile$total_visitors <- 0
shapefile$location_name <- 'NULL'
shapefile$latitude <- 0
shapefile$longitude <- 0
getUniqueMontgomeryMaps <- function(individual_POI_movement){
  listofmaps <- list()
  
  for (i in 1:length(individual_POI_movement)){
    new_map <- shapefile
    for (j in 1:length(new_map$GEOID)){
      for (k in 1:length(individual_POI_movement[[i]]$visitor_home_cbg)){
        if ((length(new_map$GEOID[j]) == length(individual_POI_movement[[i]]$visitor_home_cbg[k])) && 
            (new_map$GEOID[j] == individual_POI_movement[[i]]$visitor_home_cbg[k])){
          new_map$total_visitors[j] <- individual_POI_movement[[i]]$total_visitors[k]
        }else{}
      }
    }
    new_map$location_name[1] <- individual_POI_movement[[i]]$location_name[1]
    new_map$latitude[1] <- individual_POI_movement[[i]]$latitude[1]
    new_map$longitude[1] <- individual_POI_movement[[i]]$longitude[1]
    listofmaps[[i]] <- new_map
    print(paste(i,"/ 217"))
  }
  return(listofmaps)
}

POI_movement_maps <- list()
POI_movement_maps <- getUniqueMontgomeryMaps(individual_POI_movement)

icons <- awesomeIcons(
  icon = "ios-medkit",
  iconColor = 'blue',
  library = 'ion',
  markerColor = "red"
)
#Testing one map
domain <- subset(POI_movement_maps[[50]], total_visitors > 1)
print(domain$total_visitors)
mypallet <- colorNumeric( palette="YlOrRd", domain=domain$total_visitors, na.color='black')
choro <- leaflet(POI_movement_maps[[50]]) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    fillColor = ~mypallet(total_visitors),
    weight = 2,
    opacity = 1,
    color = 'lightblue',
    highlightOptions = highlightOptions(
      weight = 5,
      color = 'purple',
      bringToFront = TRUE),
    label = ~GEOID,
    popup = as.character(POI_movement_maps[[50]]$total_visitors)) %>%
  addAwesomeMarkers(data = POI_movement_maps[[50]],
                    ~longitude[1], ~latitude[1], icon = icons,
                    clusterOptions = markerClusterOptions(),
                    popup = ~business[1]) %>%
  setView(lng = POI_movement_maps[[50]]$longitude[1], 
          lat = POI_movement_maps[[50]]$latitude[1], 
          zoom = 10) %>%
  leaflet::addLegend(data = POI_movement_maps[[50]],
                     position = "bottomright",
                     pal = mypallet, values = ~total_visitors,
                     title = "Total Visitors",
                   opacity = 0.5)
choro

####################
#Building NAICS maps
####################
getUniqueNAICSFrames <- function(ids){
  
  listofcodes <- list()
  
  for (i in 1:length(ids)){
    sub_dat3 <- subset(overall_trips, NAICS_code == ids[i])
    sub_dat4 <- sub_dat3 %>% distinct(location_name, .keep_all = TRUE)
    sub_dat3$location_name <- 0
    sub_dat3$latitude <- 0
    sub_dat3$longitude <- 0
    for (j in 1:length(sub_dat4$location_name)){
      sub_dat3$location_name[j] <- sub_dat4$location_name[j]
      sub_dat3$latitude[j] <- sub_dat4$latitude[j]
      sub_dat3$longitude[j] <- sub_dat4$longitude[j]
    }
    listofcodes[[sub_dat3$location_name[i]]] <- sub_dat3
  }
  
  return(listofcodes)
  
}
individual_NAICS_movement <- list()
uniqueNAICS <- overall_trips %>% distinct(NAICS_code, .keep_all = TRUE)
individual_NAICS_movement <- getUniqueNAICSFrames(uniqueNAICS$NAICS_code)
View(individual_NAICS_movement[[1]])

getUniqueNAICSmaps <- function(individual_NAICS_movement){
  listofmaps <- list()
  for (i in 1:length(individual_NAICS_movement)){
    new_map <- Montgomery
    for (j in 1:length(new_map$GEOID)){
      for (k in 1:length(individual_NAICS_movement[[i]]$GEOID)){
        if ((length(new_map$GEOID[j]) == length(individual_NAICS_movement[[i]]$GEOID[k])) && 
            (new_map$GEOID[j] == individual_NAICS_movement[[i]]$GEOID[k])){
          new_map$total_visitors[j] <- (new_map$total_visitors[j] + individual_NAICS_movement[[i]]$total_visitors[k])
          print(j)
        }else{}
      }
    }
    sub_dat <- individual_NAICS_movement[[i]] %>% distinct(location_name, .keep_all = TRUE)
    for (j in 1:length(sub_dat$location_name)){
      new_map$location_name[j] <- sub_dat$location_name[j]
      new_map$latitude[j] <- sub_dat$latitude[j]
      new_map$longitude[j] <- sub_dat$longitude[j]
    }
    listofmaps[[i]] <- new_map
    print(paste(i,"/", length(individual_NAICS_movement)))
  }
  return(listofmaps)
}
NAICS_movement_maps <- list()
NAICS_movement_maps <- getUniqueNAICSmaps(individual_NAICS_movement)

#Testing one map
domain <- subset(NAICS_movement_maps[[1]], total_visitors > 1)
locations <- individual_NAICS_movement[[1]] %>% distinct(location_name, .keep_all = TRUE)
print(domain$total_visitors)
mypallet <- colorNumeric( palette="RdYlGn", domain=log10(domain$total_visitors), na.color='black')
choro <- leaflet(NAICS_movement_maps[[1]]) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    fillColor = ~mypallet(log10(total_visitors)),
    weight = 2,
    opacity = 1,
    color = 'lightblue',
    highlightOptions = highlightOptions(
      weight = 5,
      color = 'purple',
      bringToFront = TRUE),
    label = ~GEOID,
    popup = as.character(NAICS_movement_maps[[1]]$total_visitors)) %>%
  addAwesomeMarkers(locations$longitude, locations$latitude, icon = icons,
                    popup = locations$location_name) %>%
  setView(lng = median(locations$longitude), 
          lat = median(locations$latitude), 
          zoom = 10) %>%
  leaflet::addLegend(data = NAICS_movement_maps[[1]],
                     position = "bottomright",
                     pal = mypallet, values = ~total_visitors,
                     title = paste("Total Visitors", locations$NAICS_code[1]),
                     opacity = 0.5)
choro

View(locations)
#################
#Setting up shiny
#################
ui <- bootstrapPage(
  tags$style(type = "text/css", 
    
  )
)
