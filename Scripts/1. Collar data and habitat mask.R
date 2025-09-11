#1- Get collar data, dingo core territory zones, and distances to core zones. Mostly data formatting and cleaning. Also defining habitat mask for secr models. 



library(sf)
library(gdistance)
library(adehabitatHR)
library(secr)
library(dplyr)
library(alphahull)
library(ggplot2)
library(ggmap)


####First lets do 2021/2022
Andy21 <- read.csv(file = "Raw Data/Collar1/UOF1501.csv", header = T)
SWY21 <- read.csv(file = "Raw Data/Collar1/WTF1501.csv", header =T)
Cathy21 <- read.csv(file = "Raw Data/Collar1/SLF2003.csv", header = T)
RD21 <- read.csv(file = "Raw Data/Collar1/UOM2002.csv", header = T)
Tekka21 <- read.csv(file = "Raw Data/Collar1/YGM2102.csv",header=T)


# Clean column names for Tekka21
colnames(Tekka21)[colnames(Tekka21) == "Julianday"] <- "Day"
colnames(Tekka21)[colnames(Tekka21) == "NumSats"] <- "Num_of_sats"

# Get fix times into proper times for all new individuals
Andy21$DateTime <- as.POSIXct(paste(2000 + Andy21$Year, "-", Andy21$Day, " ", Andy21$Hour, ":00:00", sep = ""), format = "%Y-%j %H:%M:%S")
SWY21$DateTime <- as.POSIXct(SWY21$DateTime, format = "%Y-%m-%d %H:%M:%S")
Cathy21$DateTime <- as.POSIXct(paste(2000 + Cathy21$Year, "-", Cathy21$Day, " ", Cathy21$Hour, ":00:00", sep = ""), format = "%Y-%j %H:%M:%S")
RD21$DateTime <- as.POSIXct(paste(2000 + RD21$Year, "-", RD21$Day, " ", RD21$Hour, ":00:00", sep = ""), format = "%Y-%j %H:%M:%S")
Tekka21$DateTime <- as.POSIXct(paste(2000 + Tekka21$Year, "-", Tekka21$Day, " ", Tekka21$Hour, ":00:00", sep = ""), format = "%Y-%j %H:%M:%S")

# Remove clear outliers for new individuals
CleanAndy21 <- Andy21[Andy21$Latitude < -30, ]
CleanAndy21 <- Andy21[Andy21$Longitude < 152.22, ]

CleanSWY21 <- SWY21[SWY21$Latitude < -30, ]
CleanCathy21 <- Cathy21[Cathy21$Latitude < -30, ]
CleanRD21 <- RD21[RD21$Latitude < -30, ]
CleanTekka21 <- Tekka21[Tekka21$Latitude < -30, ]

# Remove hdop >1.5 for new individuals
#CleanAndy21 <- CleanAndy21[CleanAndy21$Hdop < 1.5, ]#DOnt run-no values.
CleanCathy21 <- CleanCathy21[CleanCathy21$Hdop < 1.5, ]
#CleanSWY21 <- CleanSWY21[CleanSWY21$Hdop <1.5,]#Dont run-no values
CleanRD21 <- CleanRD21[CleanRD21$Hdop < 1.5, ]
CleanTekka21 <- CleanTekka21[CleanTekka21$Hdop < 1.5, ]

# Plot data for new individuals- have a look if reasonable.
plot(CleanAndy21$Latitude ~ CleanAndy21$Longitude)
plot(CleanSWY21$Latitude ~ CleanSWY21$Longitude)
plot(CleanCathy21$Latitude ~ CleanCathy21$Longitude)
plot(CleanRD21$Latitude ~ CleanRD21$Longitude)
plot(CleanTekka21$Latitude ~ CleanTekka21$Longitude)

# Selecting date range preferred. If have, use.
start_date21 <- as.POSIXct("2021-12-15 00:00:00")
end_date21 <- as.POSIXct("2022-02-21 00:00:00")

Andy21Cleandates <- CleanAndy21[CleanAndy21$DateTime >= "2021-11-15 00:00:00" & CleanAndy21$DateTime <= "2022-01-30 00:00:00", ]
SWY21Cleandates<- CleanSWY21[CleanSWY21$DateTime >= "2019-05-12 00:00:00" & CleanSWY21$DateTime <= "2020-01-23 00:00:00", ]
Cathy21Cleandates <- CleanCathy21[CleanCathy21$DateTime >= start_date21 & CleanCathy21$DateTime <= end_date21, ]  # Cathy use pre-dispersal period
RD21Cleandates <- CleanRD21[CleanRD21$DateTime >= start_date21 & CleanRD21$DateTime <= end_date21, ]
Tekka21Cleandates <- CleanTekka21[CleanTekka21$DateTime >= "2022-04-14 00:00:00" & CleanTekka21$DateTime <= "2022-07-01 00:00:00", ]  # Tekka use following year

# Remove rows with NA Latitude
Andy21Cleandates <- Andy21Cleandates[!is.na(Andy21Cleandates$Latitude), ]
SWY21Cleandates <- SWY21Cleandates[!is.na(SWY21Cleandates$Latitude), ]
Cathy21Cleandates <- Cathy21Cleandates[!is.na(Cathy21Cleandates$Latitude), ]
RD21Cleandates <- RD21Cleandates[!is.na(RD21Cleandates$Latitude), ]
Tekka21Cleandates <- Tekka21Cleandates[!is.na(Tekka21Cleandates$Latitude), ]

# Add Individual column
Andy21Cleandates$Individual <- "Andy21"
SWY21Cleandates$Individual <- "SWY21"
Cathy21Cleandates$Individual <- "Cathy21"
RD21Cleandates$Individual <- "RD21"
Tekka21Cleandates$Individual <- "Tekka21"

# Combine all cleaned data
CleanAllTogether21 <- rbind(
  Andy21Cleandates[, c("DateTime", "Individual", "Latitude", "Longitude")],
  SWY21Cleandates[, c("DateTime", "Individual", "Latitude", "Longitude")],
  Cathy21Cleandates[, c("DateTime", "Individual", "Latitude", "Longitude")],
  RD21Cleandates[, c("DateTime", "Individual", "Latitude", "Longitude")],
  Tekka21Cleandates[, c("DateTime", "Individual", "Latitude", "Longitude")]
)


# Convert to sf object
combined_sf21 <- st_as_sf(CleanAllTogether21, coords = c("Longitude", "Latitude"), crs = st_crs(4326))

# Create sf objects for new individuals
Andy21points_sf <- combined_sf21[combined_sf21$Individual == "Andy21", ]
SWY21points_sf <- combined_sf21[combined_sf21$Individual == "SWY21", ]
Cathy21points_sf <- combined_sf21[combined_sf21$Individual == "Cathy21", ]
RD21points_sf <- combined_sf21[combined_sf21$Individual == "RD21", ]
Tekka21points_sf <- combined_sf21[combined_sf21$Individual == "Tekka21", ]

# Take random sample of 500 points for each new individual
random_Andy21_sf <- Andy21points_sf  ##not enough points.
random_SWY21_sf <- SWY21points_sf   #not enough point.
random_cathy21_sf <- Cathy21points_sf %>% sample_n(500)
random_rd21_sf <- RD21points_sf %>% sample_n(500)
random_tekka21_sf <- Tekka21points_sf %>% sample_n(500)

# Transform into UTM and assign spatial for these functions
random_Andy21_sp <- as(random_Andy21_sf, "Spatial")
random_SWY21_sp <- as(random_SWY21_sf, "Spatial")
random_cathy21_sp <- as(random_cathy21_sf, "Spatial")
random_rd21_sp <- as(random_rd21_sf, "Spatial")
random_tekka21_sp <- as(random_tekka21_sf, "Spatial")

Andy21PointsUTM <- spTransform(random_Andy21_sp, CRS("+init=epsg:28356"))
SWY21PointsUTM <- spTransform(random_SWY21_sp, CRS("+init=epsg:28356"))
Cathy21PointsUTM <- spTransform(random_cathy21_sp, CRS("+init=epsg:28356"))
RD21PointsUTM <- spTransform(random_rd21_sp, CRS("+init=epsg:28356"))
Tekka21PointsUTM <- spTransform(random_tekka21_sp, CRS("+init=epsg:28356"))


#Calculate Kernel utilisation distributions----#Ignore warnings
Andy21KUD <- kernelUD(Andy21PointsUTM, h = "href")
SWY21KUD <- kernelUD(SWY21PointsUTM, h = "href")
Cathy21KUD <- kernelUD(Cathy21PointsUTM, h = "href")
RD21KUD <- kernelUD(RD21PointsUTM, h ="href")
Tekka21KUD <- kernelUD(Tekka21PointsUTM, h = "href")




#Calculate KDES####
Andy21CoreKDE <- getverticeshr(Andy21KUD, percent = 50)
SWY21CoreKDE <- getverticeshr(SWY21KUD, percent =50)
Cathy21CoreKDE <- getverticeshr(Cathy21KUD, percent = 50)
RD21CoreKDE <- getverticeshr(RD21KUD, percent = 50)
Tekka21CoreKDE <- getverticeshr(Tekka21KUD, percent =50)

#Calculate KDES
Andy21PeriphKDE <- getverticeshr(Andy21KUD, percent = 95)
SWY21PeriphKDE <- getverticeshr(SWY21KUD, percent =95)
Cathy21PeriphKDE <- getverticeshr(Cathy21KUD, percent = 95)
RD21PeriphKDE <- getverticeshr(RD21KUD, percent = 95)
Tekka21PeriphKDE <- getverticeshr(Tekka21KUD, percent =95)


Andy21CoreKDE$CollarPack <- "JB"
SWY21CoreKDE$CollarPack <- "WT"
Cathy21CoreKDE$CollarPack <- "SL"
RD21CoreKDE$CollarPack <- "MB"
Tekka21CoreKDE$CollarPack <- "YG"

Andy21PeriphKDE$CollarPack <- "JB"
SWY21PeriphKDE$CollarPack <- "WT"
Cathy21PeriphKDE$CollarPack <- "SL"
RD21PeriphKDE$CollarPack <- "MB"
Tekka21PeriphKDE$CollarPack <- "YG"

#Add in name for MCP

Andy21CoreKDE$TerritoryZone <- "Core"
SWY21CoreKDE$TerritoryZone <- "Core"
Cathy21CoreKDE$TerritoryZone <- "Core"
RD21CoreKDE$TerritoryZone <- "Core"
Tekka21CoreKDE$TerritoryZone <- "Core"

Andy21PeriphKDE$TerritoryZone <- "Periph"
SWY21PeriphKDE$TerritoryZone <- "Periph"
Cathy21PeriphKDE$TerritoryZone <- "Periph"
RD21PeriphKDE$TerritoryZone <- "Periph"
Tekka21PeriphKDE$TerritoryZone <- "Periph"




#merge 

AllCore21KDE <- rbind(Andy21CoreKDE, SWY21CoreKDE,Cathy21CoreKDE, RD21CoreKDE,Tekka21CoreKDE)
AllAll21KDE <- rbind(Andy21CoreKDE, SWY21CoreKDE,Cathy21CoreKDE, RD21CoreKDE,Tekka21CoreKDE,Andy21PeriphKDE, SWY21PeriphKDE,Cathy21PeriphKDE, RD21PeriphKDE,Tekka21PeriphKDE)

#transform into sf (also change 21-->22 just for better labelling.)

AllCoresf22kde <- st_as_sf(AllCore21KDE, crs = st_crs(32756))
AllCoresf22kde <- st_transform(AllCoresf22kde, crs= 32756)

AllAllsf22kde <- st_as_sf(AllAll21KDE, crs = st_crs(32756))
AllAllsf22kde <- st_transform(AllAllsf22kde, crs= 32756)



#summaries
core_stats21 <- AllAllsf22kde %>%
  filter(TerritoryZone == "Core") %>%
  reframe(
    mean_area = mean(area),
    range_area = range(area),
    geometry = st_union(geometry)
  )

periph_stats21 <- AllAllsf22kde %>%
  filter(TerritoryZone == "Periph") %>%
  reframe(
    mean_area = mean(area),
    range_area = range(area),
    geometry = st_union(geometry)
  )



#Then repeat for 2022/2023:::

#Step one is to clean all data. 


###Read in Collar Data for individuals
Andy <- read.csv(file = "Raw Data/Collar2/UOM2008.csv", header = T)  
SWY <- read.csv(file = "Raw Data/Collar2/WTF2204.csv", header= T)
Tekka <- read.csv(file = "Raw Data/Collar2/YGM2102.csv", header= T)
Bombah <- read.csv(file = "Raw Data/Collar2/UOF1801.csv", header = T)
RedDog <- read.csv(file = "Raw Data/Collar2/UOM2002.csv", header = T)

###starting with data cleaning
colnames(Tekka)[colnames(Tekka) == "Julianday"] <- "Day"
colnames(Tekka)[colnames(Tekka) == "NumSats"] <- "Num_of_sats"
#get Tekka's fix times into proper times. 
Tekka$DateTime <- as.POSIXct(paste(2000 + Tekka$Year, "-", Tekka$Day, " ", Tekka$Hour, ":00:00", sep = ""), format = "%Y-%j %H:%M:%S")

#get SWY's fix times into proper times. 
SWY$DateTime <- as.POSIXct(paste(2000 + SWY$Year, "-", SWY$Day, " ", SWY$Hour, ":00:00", sep = ""), format = "%Y-%j %H:%M:%S")

#get Andy's fix times into proper times. 
Andy$DateTime <- as.POSIXct(paste(2000 + Andy$Year, "-", Andy$Day, " ", Andy$Hour, ":00:00", sep = ""), format = "%Y-%j %H:%M:%S")


#get Bombah's fix times into proper times. 
Bombah$DateTime <- as.POSIXct(paste(2000 + Bombah$Year, "-", Bombah$Day, " ", Bombah$Hour, ":00:00", sep = ""), format = "%Y-%j %H:%M:%S")

#get Sock's fix times into proper times. 
RedDog$DateTime <- as.POSIXct(paste(2000 + RedDog$Year, "-", RedDog$Day, " ", RedDog$Hour, ":00:00", sep = ""), format = "%Y-%j %H:%M:%S")


##remove clear outliers
CleanAndy <- Andy[(Andy$Latitude < -30), ] 
CleanSWY <- SWY[SWY$Latitude < -30, ]
CleanBombah <- Bombah[Bombah$Latitude < -31, ]
CleanTekka <- Tekka[Tekka$Latitude < -30, ]
CleanRedDog <- RedDog[RedDog$Latitude < -30, ]

#remove hdop >1.5
CleanAndy <- CleanAndy[(CleanAndy$Hdop <1.5),]
CleanSWY <- CleanSWY[(CleanSWY$Hdop <1.5),]
CleanBombah <- CleanBombah[(CleanBombah$Hdop<1.5),]
CleanTekka <- CleanTekka[(CleanTekka$Hdop<1.5),]
CleanRedDog <- CleanRedDog[(CleanRedDog$Hdop<1.5),]

plot(CleanAndy$Latitude ~ CleanAndy$Longitude)
plot(CleanSWY$Latitude ~ CleanSWY$Longitude)
plot(CleanBombah$Latitude ~ CleanBombah$Longitude)
plot(CleanTekka$Latitude ~ CleanTekka$Longitude)
plot(CleanRedDog$Latitude ~ CleanRedDog$Longitude)

#selecting date range preferred. If have, use. 
start_date <- as.POSIXct("2022-12-05 00:00:00")
end_date <- as.POSIXct("2023-02-21 00:00:00")

AndyCleandates<- CleanAndy[CleanAndy$DateTime >= start_date & CleanAndy$DateTime <= end_date, ] #good for andy
SWYCleandates<- CleanSWY[CleanSWY$DateTime >= "2023-12-05" & CleanSWY$DateTime <= "2024-01-21", ] #good for SWY
BombahCleandates<- CleanBombah[CleanBombah$DateTime >= start_date & CleanBombah$DateTime <= end_date, ] #Good for bombah
TekkaCleandates <- CleanTekka[CleanTekka$DateTime >= "2022-04-14 00:00:00" & CleanTekka$DateTime <= "2022-07-01 00:00:00", ]  # Tekka use previous year
RedDogCleandates <- CleanRedDog[CleanRedDog$DateTime >= start_date & CleanRedDog$DateTime <= end_date, ]


BombahCleandates <- BombahCleandates[-which(is.na(BombahCleandates$Latitude)),]
#SWYCleandates <- SWYCleandates[-which(is.na(SWYCleandates$Latitude)),]Don't run, none
#AndyCleandates <- AndyCleandates[-which(is.na(AndyCleandates$Latitude)),] dont run, none. 
#TekkaCleandates <- TekkaCleandates[-which(is.na(TekkaCleandates$Latitude)),]#don't run 
#RedDogCleandates <- RedDogCleandates[-which(is.na(RedDogCleandates$Latitude)),]#don't run, none. 

  



AndyCleandates$Individual <- "Andy"
SWYCleandates$Individual <- "SWY"
BombahCleandates$Individual <- "Bombah"
TekkaCleandates$Individual <- "Tekka"
RedDogCleandates$Individual <- "RedDog"

CleanAllTogether <- rbind(
  AndyCleandates[, c("DateTime", "Individual", "Latitude", "Longitude", "Num_of_sats")],
  SWYCleandates[, c("DateTime", "Individual", "Latitude", "Longitude", "Num_of_sats")],
  BombahCleandates[, c("DateTime", "Individual", "Latitude", "Longitude", "Num_of_sats")],
  TekkaCleandates[, c("DateTime", "Individual", "Latitude", "Longitude", "Num_of_sats")],
  RedDogCleandates[, c("DateTime", "Individual", "Latitude", "Longitude", "Num_of_sats")])


CleanAllTogether <- CleanAllTogether%>%
  filter(!is.na(Longitude))

##plotting all together
ggplot(CleanAllTogether, aes(x = Longitude, y = Latitude, color = Individual)) +
  geom_point() +
  labs(x = "Longitude", y = "Latitude", title = "GPS Coordinates with Individual Coloring") +
  theme_minimal()
#Data looks pretty nice. 

#make sf object
combined_sf <- st_as_sf(CleanAllTogether, coords = c("Longitude", "Latitude"), crs = st_crs(4326))
#make each dataframe sf object too 

Andypoints_sf<- combined_sf[combined_sf$Individual == "Andy",]
SWYpoints_sf<- combined_sf[combined_sf$Individual == "SWY",] 
Bombahpoints_sf <- combined_sf[combined_sf$Individual == "Bombah",]
Tekkapoints_sf <- combined_sf[combined_sf$Individual == "Tekka",]
RedDogpoints_sf <- combined_sf[combined_sf$Individual == "RedDog",]


#Take random sample of 500 points for each 

random_andy_sf <- Andypoints_sf %>% sample_n(500)
random_swy_sf <- SWYpoints_sf %>% sample_n(500)
random_bombah_sf <- Bombahpoints_sf %>% sample_n(500)
random_tekka_sf <- Tekkapoints_sf %>% sample_n(500)
random_RedDog_sf <- RedDogpoints_sf %>% sample_n(500)




#transform into utm and assign spatial for these functions.... 

random_andy_sp <- as(random_andy_sf, "Spatial")
random_SWYpoints_sp <- as(random_swy_sf, "Spatial")
random_Bombahpoints_sp <- as(random_bombah_sf, "Spatial")
random_Tekkapoints_sp <- as(random_tekka_sf, "Spatial")
random_RedDogpoints_sp <- as(random_RedDog_sf, "Spatial")

AndyPointsUTM <- spTransform(random_andy_sp, CRS("+init=epsg:28356"))
SWYPointsUTM <- spTransform(random_SWYpoints_sp, CRS("+init=epsg:28356"))
BombahPointsUTM <- spTransform(random_Bombahpoints_sp, CRS("+init=epsg:28356"))
TekkaPointsUTM <- spTransform(random_Tekkapoints_sp, CRS("+init=epsg:28356"))
RedDogPointsUTM <- spTransform(random_RedDogpoints_sp, CRS("+init=epsg:28356"))



#Data is cleaned- now we are going to estimate the 50% MCPs



#Calculate Kernel utilisation distributions----#Ignore warnings
#Calculate Kernel utilisation distributions----#Ignore warnings
Andy23KUD <- kernelUD(AndyPointsUTM, h = "href")
SWY23KUD <- kernelUD(SWYPointsUTM, h = "href")
RD23KUD <- kernelUD(RedDogPointsUTM, h = "href")
Bombah23KUD <- kernelUD(BombahPointsUTM, h ="href")
Tekka23KUD <- kernelUD(TekkaPointsUTM, h = "href")




#Calculate KDES####
Andy23CoreKDE <- getverticeshr(Andy23KUD, percent = 50)
SWY23CoreKDE <- getverticeshr(SWY23KUD, percent =50)
RD23CoreKDE <- getverticeshr(RD23KUD, percent = 50)
Bombah23CoreKDE <- getverticeshr(Bombah23KUD, percent = 50)
Tekka23CoreKDE <- getverticeshr(Tekka23KUD, percent =50)

#Calculate KDES
Andy23PeriphKDE <- getverticeshr(Andy23KUD, percent = 95)
SWY23PeriphKDE <- getverticeshr(SWY23KUD, percent =95)
RD23PeriphKDE <- getverticeshr(RD23KUD, percent = 95)
Bombah23PeriphKDE <- getverticeshr(Bombah23KUD, percent = 95)
Tekka23PeriphKDE <- getverticeshr(Tekka23KUD, percent =95)




#Give them names 


Andy23CoreKDE$CollarPack <- "JB"
SWY23CoreKDE$CollarPack <- "WT"
RD23CoreKDE$CollarPack <- "SL"
Bombah23CoreKDE$CollarPack <- "MB"
Tekka23CoreKDE$CollarPack <- "YG"

Andy23PeriphKDE$CollarPack <- "JB"
SWY23PeriphKDE$CollarPack <- "WT"
RD23PeriphKDE$CollarPack <- "SL"
Bombah23PeriphKDE$CollarPack <- "MB"
Tekka23PeriphKDE$CollarPack <- "YG"



#Give them names 

Andy23CoreKDE$TerritoryZone <- "Core"
SWY23CoreKDE$TerritoryZone <- "Core"
RD23CoreKDE$TerritoryZone <- "Core"
Bombah23CoreKDE$TerritoryZone <- "Core"
Tekka23CoreKDE$TerritoryZone <- "Core"

Andy23PeriphKDE$TerritoryZone <- "Periph"
SWY23PeriphKDE$TerritoryZone <- "Periph"
RD23PeriphKDE$TerritoryZone <- "Periph"
Bombah23PeriphKDE$TerritoryZone <- "Periph"
Tekka23PeriphKDE$TerritoryZone <- "Periph"



AllCoreKde <- rbind(Andy23CoreKDE,SWY23CoreKDE,RD23CoreKDE,Bombah23CoreKDE,Tekka23CoreKDE)

AllAllKde <- rbind(Andy23CoreKDE,SWY23CoreKDE,RD23CoreKDE,Bombah23CoreKDE,Tekka23CoreKDE,Andy23PeriphKDE,SWY23PeriphKDE,RD23PeriphKDE,Bombah23PeriphKDE,Tekka23PeriphKDE)


AllAllCoreKdesf23 <- st_as_sf(AllCoreKde)
AllAllCoreKdesf23 <- st_transform(AllAllCoreKdesf23, crs = 32756)


AllAllKdesf23 <- st_as_sf(AllAllKde)
AllAllKdesf23 <- st_transform(AllAllKdesf23, crs = 32756)


plot(AllAllKdesf23)

###Next step is to create the habitat mask. We can then compute the distances of each point in the mask, to each of the core zones. 




#read in shapefile of area, which has the habitat removed. 
study_area_clipped <- sf::read_sf("Raw Data/studyareaclipped.shp")

#removing points below smiths-no cameras further north. 
get_max_northing <- function(geometry) {
  max(st_coordinates(geometry)[, 2])
}

max_northing_values <- sapply(study_area_clipped$geometry, get_max_northing)
study_area_clipped <- study_area_clipped[max_northing_values < 6415777.58, ]

#we'll do a slight buffer to improve slight curly mask edges
study_area_clipped <- st_buffer(study_area_clipped, 50) #do by small amount to ensure polygons overlap each other. 
study_area_clipped <- st_union(study_area_clipped, dTolerance = 20) #unionise to make them one.
study_area_clipped <- st_buffer(study_area_clipped, 100) #buffer by slightly more. 


trapsall2023 <- read.csv(file = "Raw Data/UTMtraps.csv", header = TRUE)

traps2023 <- read.traps(data = trapsall2023, detector = "proximity", trapID = "Trap")

trapsall <- read.csv(file = "Raw Data/TrapsUTMSpecific.csv", header = T)

trapsall2022 <- trapsall%>%
  filter(Session==1 & !Trap=='PS24' & !Trap=='PS23')%>%
  select(Trap,x,y)


trapsall2023 <- trapsall%>%
  filter(Session==2 & !Trap=='PS26')%>%
  select(Trap,x,y)

traps2022 <- read.traps(data = trapsall2022, detector = "proximity", trapID = "Trap")
traps2023 <- read.traps(data = trapsall2023, detector = "proximity", trapID = "Trap")






# Use trapsfile we made, and then the study area clipped. 

maskclippedforpaper2023 <- make.mask(traps2023, 
                                 type = 'trapbuffer', 
                                 buffer = 5000,
                                 poly = study_area_clipped, 
                                 keep.poly = F, 
                                 nx = 90, 
                                 ny = 90)

maskclippedforpaper2022 <- make.mask(traps2022, 
                                     type = 'trapbuffer', 
                                     buffer = 5000,
                                     poly = study_area_clipped, 
                                     keep.poly = F, 
                                     nx = 90, 
                                     ny = 90)


#Have a look
plot(maskclippedforpaper2022)
points(traps2022)

#make into just points for data manipulation 
maskfinalpoints <- as.data.frame(maskclippedforpaper2023)

#and a dummy sacrifice sf object

#make sf object
mask1sf <- st_as_sf(maskclippedforpaper2023, coords = c("x", "y"), crs = st_crs(32756))




###Getting distances to centre of core ranges. 
#first get points of the mask 
sf_maskpointsreal23 <- st_centroid(AllAllCoreKdesf23)
sf_maskpointsreal22 <- st_centroid(AllCoresf22kde)

#23
maskpintsreal23 <- as.data.frame(sf_maskpointsreal23$geometry)
maskpintsreal23coords <- st_coordinates(sf_maskpointsreal23)

#note - need to slightly adjust MB pack, as it's slightly in the water, producing infinities for distance. will do manually. 
maskpintsreal23coords[4, "X"]<- 436443.28
maskpintsreal23coords[4, "Y"]<- 6400454.62


maskpintsreal23 <- data.frame(x = maskpintsreal23coords[, "X"], y = maskpintsreal23coords[, "Y"])
maskpintsreal23$Pack <- factor(c("JB", "WT", "SL", "MB", "YG"))
points(maskpintsreal23)
(maskpintsreal23)

#22
maskpintsreal22 <- as.data.frame(sf_maskpointsreal22$geometry)

#note - need to slightly adjust MB pack, as it's slightly in the water, producing infinities for distance. will do manually. 



maskpintsreal22coords <- st_coordinates(sf_maskpointsreal22)
maskpintsreal22coords[4, "X"]<- 436662.46
maskpintsreal22coords[4, "Y"]<- 6401123.38

maskpintsreal22 <- data.frame(x = maskpintsreal22coords[, "X"], y = maskpintsreal22coords[, "Y"])
maskpintsreal22$Pack <- factor(c("JB", "WT", "SL", "MB", "YG"))
points(maskpintsreal22)
(maskpintsreal22)

maskpointsbloodyhell23 <- as.data.frame(maskclippedforpaper2023)
maskpointsbloodyhell22 <- as.data.frame(maskclippedforpaper2023)

##Get points from mask- although maybe don't even need to do this. 
maskpointagain23 <- as.data.frame(maskclippedforpaper2023)
maskpointagain22 <- as.data.frame(maskclippedforpaper2023)

distancetocore23 <-as.data.frame(nedist(maskclippedforpaper2023, maskpintsreal23coords, maskclippedforpaper2023))

distancetocore22 <-as.data.frame(nedist(maskclippedforpaper2023, maskpintsreal22coords, maskclippedforpaper2023))

#get minimum and select only that row. 
distancetocore23$min <-apply(distancetocore23, 1, min)
distancetocore23 <- distancetocore23[, c(6)]

distancetocore22$min <-apply(distancetocore22, 1, min)
distancetocore22 <- distancetocore22[, c(6)]

attr(maskclippedforpaper2023,"covariates") <- NULL

mask1sf$distancetocore23 <- distancetocore23
mask1sf$distancetocore22 <- distancetocore22

###Check if needed, hopefully not
sum(is.infinite(mask1sf$distancetocore22))

####This time not neededmask1sf$distancetocore23 <- ifelse(mask1sf$distancetocore23 == Inf, 10000, mask1sf$distancetocore23)

####This time not neededmask1sf$distancetocore22 <- ifelse(mask1sf$distancetocore22 == Inf, 10000, mask1sf$distancetocore22)


maskclippedforpaper2023 <- addCovariates(maskclippedforpaper2023, mask1sf['distancetocore23'])

maskclippedforpaper2023 <- addCovariates(maskclippedforpaper2023, mask1sf['distancetocore22'])


#Done core distance good! 

par(mfrow=c(1,2))

plot(maskclippedforpaper2023,covariate = "distancetocore23", legend = FALSE)
points(maskpintsreal23)
plot(maskclippedforpaper2023,covariate = "distancetocore22", legend = FALSE)
points(maskpintsreal22)


#Done- got  distance metric into the mask. look good. 


#installing old package of SECR, coz of bug in latest version. 
packageurl <- "https://cran.r-project.org/src/contrib/Archive/secr/secr_4.6.7.tar.gz"
install.packages(packageurl, repos=NULL, type="source")



#Now we will use the spatial polygon to extract if either 'core' or 'peripheral' territory. 

##doing it again

##Get points from mask- although maybe don't even need to do this. 
maskpointagain23 <- as.data.frame(maskclippedforpaper2023)
maskpointagain22 <- as.data.frame(maskclippedforpaper2023)

mask_points <- st_as_sf(maskclippedforpaper2023, coords = c("x", "y"))
st_crs(mask_points) <- 32756

intersection_result <- st_intersection(mask_points, AllAllKdesf23)

# Create a covariate vector the same length as mask points
# Initialize with NA values
covariate_values <- rep(NA, nrow(mask_points))

# Get the row indices of mask points that intersected with polygons
intersected_indices <- as.numeric(rownames(intersection_result))

covariate_values[intersected_indices] <- intersection_result$TerritoryZone

maskprac23 <- addCovariates(maskclippedforpaper2023, covariate_values)








#use dummy mask 
maskprac23 <- maskclippedforpaper2023

maskprac23 <- addCovariates(maskclippedforpaper2023,AllAllKdesf23)

territoryvalues23 <- as.data.frame(covariates(maskprac23)$TerritoryZone)

territoryvalues23[is.na(territoryvalues23)]<- "None"

colnames(territoryvalues23)[colnames(territoryvalues23)=="covariates(maskprac23)$TerritoryZone"] <- "TerritoryZone23"

#use dummy mask 
maskprac22 <- maskclippedforpaper2023
maskprac22 <- addCovariates(maskprac22,AllAllsf22kde)


territoryvalues22 <- as.data.frame(covariates(maskprac22)$TerritoryZone)
territoryvalues22[is.na(territoryvalues22)]<- "None"

colnames(territoryvalues22)[colnames(territoryvalues22)=="covariates(maskprac22)$TerritoryZone"] <- "TerritoryZone22"


#This is a bit of a roundabout way, but just to not stuff up the proper mask somehow. 




attr(maskclippedforpaper2023, "covariates") <- NULL

covariates(maskclippedforpaper2023)<- territoryvalues23

mask1sf$TerritoryZone22 <- territoryvalues22$TerritoryZone

maskclippedforpaper2023 <- addCovariates(maskclippedforpaper2023, mask1sf['TerritoryZone22'])


maskclippedforpaper2023 <- addCovariates(maskclippedforpaper2023, mask1sf['distancetocore23'])

maskclippedforpaper2023 <- addCovariates(maskclippedforpaper2023, mask1sf['distancetocore22'])


#look at this if you want:
par(mfrow=c(1,1))
plot(maskclippedforpaper2023, covariate = "TerritoryZone22")
plot(maskclippedforpaper2023, covariate = "TerritoryZone23")
plot(maskclippedforpaper2023, covariate = "distancetocore23",legend = FALSE)
plot(maskclippedforpaper2023, covariate = "distancetocore22",legend = FALSE)
#all finished adding covariates to mask now. 

#proceed with models. 


