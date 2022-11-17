
library(knitr)
library(tidyverse)
library(dplyr)
library(sf)
library(sp)
library(tidycensus)
library(RSocrata)
library(spdep)
library(spatstat)
library(RColorBrewer)
library(viridis)
library(classInt)
library(reshape2)
library(FNN)
library(tm)
library(raster)
library(geojsonsf)


#===========================================================================================#
#                                   Read in Data 
#===========================================================================================#

maricopa_crs <- 'EPSG:2223'
census_api_key("0f5f47c1f6197eb4d9a922410ed9dfb8af738b50", overwrite = TRUE) 

#Boundary/outline of mesa

mesa_boundary <- st_as_sf(st_read("City_Boundary.csv"), wkt = 'Geometry', crs = 4326, agr = 'constant')%>%st_transform(maricopa_crs)

#------------------------------------  Census Data  -----------------------------------------------#

#Census Block group data: geometries + sociodemographic data

variables_of_interest2 <- c("pop" = "B01003_001",
                            "white_pop" = "B03002_003",
                            "black_pop" = "B03002_004",
                            "hispanic_pop" = "B03002_012",
                            "median_income" = "B19013_001",
                            "unemployed" = "B23025_005",
                            "total_labor_force" = "B23025_003",
                            "owner_occupied_homes" = "B25003_002",
                            "renter_occupied_homes" = "B25003_003",
                            "vacant_homes" = "B25002_003",
                            "total_homes" = "B25003_001"
)

mesa_cb_groups <- get_acs(geography = "block group", variables = variables_of_interest2, 
                          year=2020, survey = "acs5", state=04, county=013, geometry=TRUE) %>% st_transform(maricopa_crs)

out_of_bounds <- c('040139413004', '040139413002', '040130101021', '040138169001', '040139413003', '1040133184002')

mesa_cb_groups<- mesa_cb_groups[ ! mesa_cb_groups$GEOID %in% out_of_bounds, ]
mesa_cb_groups <- mesa_cb_groups[mesa_boundary,]

mesa_cb_table <- mesa_cb_groups %>%
  dplyr::select( -NAME, -moe) %>% spread(variable, estimate) %>%
  mutate(area = st_area(geometry),
         perc_white = white_pop/pop,
         perc_black = black_pop/pop,
         perc_hispanic = hispanic_pop/pop,
         perc_unemployed = unemployed/total_labor_force,
         perc_owners = owner_occupied_homes/total_homes,
         perc_renters = renter_occupied_homes/total_homes,
         perc_vacant = vacant_homes/total_homes)

mesa_cb_table <- na.omit(mesa_cb_table)

#----------------------------------  Overdose Dataset  ------------------------------------------#


overdoses <- st_as_sf(na.omit(read.socrata("https://data.mesaaz.gov/resource/qufy-tzv6.json")), 
                      coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>% 
  st_transform(st_crs(maricopa_crs))

overdoses18_20 <- overdoses[overdoses$year == 2018 | overdoses$year == 2019 | overdoses$year == 2020, ]

overdose_intersect <- overdoses18_20 %>% st_join(., mesa_cb_table, join=st_within) %>%
  group_by(GEOID) %>% summarize(count = n()) %>% st_drop_geometry() 
colnames(overdose_intersect)[2] <- "overdose_count"

mesa_cb_table <- left_join(mesa_cb_table, overdose_intersect, by="GEOID")
mesa_cb_table$overdose_count[is.na(mesa_cb_table$overdose_count)] <- 0


##### #plot choropleth of overdoses by quantile

mesa_cb_chor <- mutate(mesa_cb_table, overdose_cat = cut(overdose_count, breaks = c(-.000001, 1, 3, 6, 10, 14, 45))) 

ggplot(mesa_cb_chor) + 
  geom_sf(aes(fill=overdose_cat)) +
  scale_fill_brewer(palette = "OrRd") 

pal <- brewer.pal(6, "OrRd")
plot(mesa_cb_chor[, "overdose_count"],
     breaks = c(-.000001, 1, 3, 6, 10, 14, 45),
     pal = pal,
     main = "Overdoses in Mesa (2018-2020) by Census Block Group")


#-------------------------------------  Zoning Dataset --------------------------------------------#

zoning_parcels <- st_read("Zoning Districts.geojson")%>%st_transform(st_crs(maricopa_crs))

zones <- c("resid_high", "resid_low", "CBD", "commercial")

zoning_parcels$zoning[str_detect(zoning_parcels$zoning, "^RM")] <- "resid_high"
zoning_parcels$zoning[str_detect(zoning_parcels$zoning, "^RS")] <- "resid_low"
zoning_parcels$zoning[str_detect(zoning_parcels$zoning, "^D")] <- "CBD"
commercial_list <- c("GC","LC","NC","OC")
zoning_parcels$zoning[zoning_parcels$zoning %in%commercial_list] <- "commercial"
zoning_parcels <- zoning_parcels[zoning_parcels$zoning %in% zones, ]
zoning_parcels <- zoning_parcels[c("zoning","geometry")]

mesa_cb_sub <- subset(mesa_cb_table, select = c(GEOID, geometry, area))
zoning_intersection <- st_intersection(mesa_cb_sub, st_make_valid(zoning_parcels))
zoning_intersection$inter_area <- st_area(zoning_intersection$geometry)

zoning_intersection <- aggregate(zoning_intersection$inter_area, 
                                 by=list(zoning_intersection$GEOID,zoning_intersection$zoning), FUN=sum)

zoning_intersection<-dcast(zoning_intersection, Group.1~Group.2)

zoning_intersection[is.na(zoning_intersection)] <- 0
colnames(zoning_intersection)[1] <- "GEOID"

mesa_cb_table <- left_join(mesa_cb_table, zoning_intersection,by="GEOID")
mesa_cb_table[is.na(mesa_cb_table)] <- 0

mesa_cb_table <- mesa_cb_table %>% mutate(
  cbd_per = as.numeric(CBD/area*100),
  resid_high_per = as.numeric(resid_high/area*100),
  resid_low_per = as.numeric(resid_low/area*100),
  commercial_per = as.numeric(commercial/area*100))

columns_to_keep <- c("GEOID", "pop", "area", "overdose_count", "perc_white", "perc_black", "perc_hispanic", 
                     "perc_unemployed", "perc_owners", "perc_renters", "perc_vacant",
                     "cbd_per", "resid_high_per", "resid_low_per", "commercial_per", "geometry")

mesa_cb_updated <- mesa_cb_table[columns_to_keep]


#------------------------------  City Property + Graffiti Dataset ------------------------------------------#

city_prop<- st_as_sf(na.omit(read.socrata("https://data.mesaaz.gov/resource/xms2-ya86.json")),
                     coords = c("longitude", "latitude"), crs = 4326, agr = "constant")%>%st_transform(st_crs(maricopa_crs))

city_prop <- city_prop %>% distinct(address, .keep_all = TRUE)

# public safety + greenspace + vacant property + community dev + graffiti

safety <- city_prop%>%filter(property_use %in% c("Public Safety--Fire/Police", "Park/Public Safety"))%>%
  dplyr::select(property_use, geometry)
safety$property_use <- "public_safety"

green<- city_prop%>%filter(property_use %in% c("Parks", "Pocket Park", "Cemetery"))%>%
  dplyr::select(property_use, geometry)
green$property_use <- "public_reenspace"

vacant_city<- city_prop%>%filter(property_use %in% c("Vacant", "Vacant (ADOT remnant)"))%>%
  dplyr::select(property_use, geometry)
vacant_city$property_use <- "vacant_property"

community <- city_prop%>%filter(
  property_use %in% c("Libraries", "Mesa Arts Center","Museums","Child Crisis Center", "Sequoia Charter School", "Senior Center"))%>%
  dplyr::select(property_use, geometry)
community$property_use <- "comm_development"


graffiti_calls <- st_as_sf(na.omit(read.socrata("https://data.mesaaz.gov/resource/9spb-749m.json")),
                           coords = c("lon", "lat"), crs = 4326, agr = "constant")%>%st_transform(st_crs(maricopa_crs))

graffiti_calls$nearest_address <- removeNumbers(graffiti_calls$nearest_address)
graffiti_calls$date_reported <- as.Date(graffiti_calls$date_reported)
graffiti_calls$property_use <- "graffiti"

graffiti_calls <- graffiti_calls %>% distinct(date_reported, nearest_address, .keep_all = TRUE) %>% 
  filter(between(date_reported, as.Date('2018-01-01'), as.Date('2021-01-01'))) %>%
  dplyr::select(property_use, geometry)


mesa_cb_updated <- 
  rbind(safety, green, vacant_city, community, graffiti_calls) %>%
  st_join(., mesa_cb_updated, join=st_within) %>%
  st_drop_geometry() %>%
  group_by(GEOID,property_use) %>%
  summarize(count = n()) %>%
  full_join(mesa_cb_updated) %>%
  spread(property_use, count, fill=0) %>%
  st_sf() %>%
  dplyr::select(-`<NA>`) %>%
  na.omit() %>%
  ungroup()


### nearest neighbor function from Public Policy Analytics Textbook

nn_function <- function(measureFrom,measureTo,k) {
  measureFrom_Matrix <-
    as.matrix(measureFrom)
  measureTo_Matrix <-
    as.matrix(measureTo)
  nn <-   
    get.knnx(measureTo, measureFrom, k)$nn.dist
  output <-
    as.data.frame(nn) %>%
    rownames_to_column(var = "thisPoint") %>%
    gather(points, point_distance, V1:ncol(.)) %>%
    arrange(as.numeric(thisPoint)) %>%
    group_by(thisPoint) %>%
    summarize(pointDistance = mean(point_distance)) %>%
    arrange(as.numeric(thisPoint)) %>% 
    dplyr::select(-thisPoint) %>%
    pull()
  return(output) 
}

### finding distance (nearest neighbors) to point data

st_c <- st_coordinates
st_coid <- st_centroid

mesa_cb_updated <-
  mesa_cb_updated %>%
  mutate(
    dist_public_safety =
      nn_function(st_c(st_coid(mesa_cb_updated)), st_c(safety),3),
    dist_greenspace =
      nn_function(st_c(st_coid(mesa_cb_updated)), st_c(green),3),
    dist_vacant_prop =
      nn_function(st_c(st_coid(mesa_cb_updated)), st_c(vacant_city),3),
    dist_comm_dev =
      nn_function(st_c(st_coid(mesa_cb_updated)), st_c(community),3),
    dist_graffiti =
      nn_function(st_c(st_coid(mesa_cb_updated)), st_c(graffiti_calls),3))


#----------------------------------  Police Incidents Dataset  ---------------------------------------#


incidents <- st_as_sf(na.omit(read.socrata("https://data.mesaaz.gov/resource/39rt-2rfj.json")),
                      coords = c("longitude", "latitude"), crs = 4326, agr = "constant")%>%
  st_transform(st_crs(maricopa_crs))%>%
  filter(report_year == '2018' | report_year == '2019' | report_year == '2020')
incidents <- incidents[mesa_boundary,]

notapp <- c("DEATH", "DUI", "FOUND", "COLLISION", "INSURANCE", "LICENSE", "LOST", "PARKING", "MEDICAL", 
            "SUSPENSION", "DOG", "ANIMAL", "SPEED", "IGNITION", "REGISTRATION", "FAIL", "MENTALLY")
incidents <- incidents[ ! grepl(paste(notapp, collapse = "|"), incidents$crime_type), ]
incidents <- incidents[c("crime_id","geometry")]

incidents_intersect <- incidents %>% st_join(., mesa_cb_updated, join=st_within) %>%
  group_by(GEOID) %>% summarize(count = n()) %>% st_drop_geometry() 
colnames(incidents_intersect)[2] <- "arrests_count"

mesa_cb_final <- left_join(mesa_cb_updated, incidents_intersect, by="GEOID")
mesa_cb_final$arrests_count[is.na(mesa_cb_final$arrests_count)] <- 0


####################### write data to a geojson #########################

st_write(mesa_cb_final, "mesa_complete_data.geojson")

