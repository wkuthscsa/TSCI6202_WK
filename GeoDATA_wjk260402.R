# (Run install.packages(c("sf", "tigris", "dplyr", "ggplot2", "rio")) if needed)
library(sf)         # The modern standard for spatial data in R
library(tigris)     # Downloads US Census shapefiles directly (no manual unzipping!)
library(dplyr)      # Data manipulation
library(ggplot2)    # We use the same ggplot2 grammar for maps via geom_sf()
library(rio)        # For importing CSVs
library(tidygeocoder)
library(tmap)
texascounties <- counties(state = "TX", cb = TRUE)
ggplot(data=texascounties)+
  geom_sf()
fqhc_url<-"https://data.hrsa.gov/DataDownload/DD_Files/Health_Center_Service_Delivery_and_LookAlike_Sites.xlsx"
fqhc<-import(fqhc_url)
fqhc_sf<-subset(fqhc,`Site State Abbreviation`=="TX") %>% 
  st_as_sf(coords = c("Geocoding Artifact Address Primary X Coordinate", "Geocoding Artifact Address Primary Y Coordinate"), crs = 4326)

fqhc_county<-st_transform(fqhc_sf,crs=st_crs(texascounties))
fqhc_county2<-st_join(fqhc_county,texascounties) %>% 
  group_by(NAME) %>% summarize(clinic_count_raw=n()) %>% 
  st_drop_geometry() %>% 
  left_join(texascounties,.,by="NAME") %>% 
  mutate(clinic_count=coalesce(log(clinic_count_raw),0),clinic_count_raw=coalesce(clinic_count_raw,0))



ggplot(data=fqhc_county2)+
  geom_sf(aes(fill=clinic_count))+
  geom_sf(data=fqhc_sf, color="pink", size=1.5, alpha=0.3)+
  scale_fill_viridis_c()

#Creating Synthetic Data of Street Addresses
street_address<-data.frame(full_address=c("7703 Floyd Curl Dr, San Antonio, TX 78258","1 Haven for Hope Way, San Antonio, TX 78207", "300 Alamo Plaza, San Antonio, TX 78205", "1402 Broadway St, Galveston, TX 77550", "411 Elm St, Dallas, TX 75202", "604 Brazos St, Austin, TX 78701", "2515 W 5th St, Irving, TX 75060", "100 Lady Bird Lane, Johnson City, TX 78636", "1412 W Ohio Ave, Midland, TX 79701", "1217 W Sam Rayburn Dr, Bonham, TX 75418", "3700 Hogge Dr, Parker, TX 75002", "2101 Ross Ave, Dallas, TX 75201", "2414 Rosedale St, Houston, TX 77004"))
#Obtain cordinantes for the addressses
geocoded_street_address<-geocode(street_address, address=full_address, method="osm")
#Converting coordinates to shape file
shape_file_street_address<-st_as_sf(geocoded_street_address,coords=c("long","lat"),crs=4326)

ggplot(data=fqhc_county2)+
  geom_sf(fill="purple")+
  geom_sf(data=fqhc_sf, color="pink", size=1.5, alpha=0.3)+
  geom_sf(data=shape_file_street_address, color="blue")



#Interactive Mapping
tmap_mode("view")
tm_shape(fqhc_county2)+
  tm_polygons(alpha=0.1,id="clinic_count_raw")+
  tm_shape(fqhc_sf,color="red")

View(fqhc_county2)
