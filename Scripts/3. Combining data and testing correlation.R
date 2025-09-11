#3- Combining variables into one dataset, and plotting the results and running models
library(secr)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(viridis)
library(patchwork)
library(optimix)
library(lme4)
library(tidyverse)
library(SpatialKDE)
library(sf)
library(tmap)
library(raster)
#Lets look at the predicted activity centres of the dingoes. 

activecentres2023 <- fxTotal(HN2023)
activecentres2022 <- fxTotal(HN2022)

par(mfrow=c(1,2))

plot(activecentres2023, covariate = "D.sum")
plot(activecentres2022, covariate = "D.sum")


valuescentres2023 <- data.frame(activecentres2023)

valuescentres2022 <- data.frame(activecentres2022)




par(mfrow=c(1,2))
break23 <- c(0, 0.008, 0.03)
break22 <- c(0,0.006,0.02)

# Define custom colors
colors <- c("lightgrey", "darkgreen")

par(mfrow=c(1,2))

# Plot for 2023
plot(activecentres2023, covariate = "D.sum", 
     breaks = break23, 
     col = colors,legend = FALSE)

# Plot for 2022
plot(activecentres2022, covariate = "D.sum", 
     breaks = break22, 
     col = colors,
     legend = FALSE)

par(mfrow=c(1,1))
plot(maskclippedforpaper2023, covariate = "TerritoryZone22")
points(traps2023)
plot(maskclippedforpaper2023,covariate = "TerritoryZone23")

#lovely

bloodybloodyhell <- raster(maskprac22)
bloodybloodyhell <- as(bloodybloodyhell, "SpatialPolygons")
bloodybloodyhell <- st_as_sf(bloodybloodyhell)
st_crs(bloodybloodyhell) <- 32756 

covariates2023 <- attr(activecentres2023, "covariates")
covariates2022 <- attr(activecentres2022, "covariates")


covariatesyep23 <- attr(maskclippedforpaper2023, "covariates")
covariatesyep22 <- attr(maskclippedforpaper2023, "covariates")



valuescentres2023$D.sum <- covariates2023$D.sum
valuescentres2023$Zone <- covariatesyep23$TerritoryZone23
valuescentres2023$distancetocore23 <- covariatesyep23$distancetocore23

valuescentres2022$D.sum <- covariates2022$D.sum
valuescentres2022$Zone <- covariatesyep22$TerritoryZone22
valuescentres2022$distancetocore22 <- covariatesyep23$distancetocore22


###got the dataset. 




#So lets split them first. 

valuescentres2023 <- valuescentres2023 %>%
  mutate(D.sum.category = case_when(
    D.sum <= 0.002 ~ "0-0.002",
    D.sum > 0.002 & D.sum <= 0.004 ~ "0.002-0.004",
    D.sum > 0.004 & D.sum <= 0.006 ~ "0.004-0.006",
    D.sum > 0.006 & D.sum <= 0.008 ~ "0.006-0.008",
    D.sum > 0.008 & D.sum <= 0.010 ~ "0.008-0.010",
    D.sum > 0.010 & D.sum <= 0.012 ~ "0.010-0.012",
    D.sum > 0.012 & D.sum <= 0.014 ~ "0.012-0.014",
    D.sum > 0.014 & D.sum <= 0.016 ~ "0.014-0.016",
    TRUE ~ ">0.016"),
         Year = "2023")%>%
  rename(distancetocore = distancetocore23)

valuescentres2022 <- valuescentres2022 %>%
  mutate(D.sum.category = case_when(
    D.sum <= 0.002 ~ "0-0.002",
    D.sum > 0.002 & D.sum <= 0.004 ~ "0.002-0.004",
    D.sum > 0.004 & D.sum <= 0.006 ~ "0.004-0.006",
    D.sum > 0.006 & D.sum <= 0.008 ~ "0.006-0.008",
    D.sum > 0.008 & D.sum <= 0.010 ~ "0.008-0.010",
    D.sum > 0.010 & D.sum <= 0.012 ~ "0.010-0.012",
    D.sum > 0.012 & D.sum <= 0.014 ~ "0.012-0.014",
    D.sum > 0.014 & D.sum <= 0.016 ~ "0.014-0.016",
    TRUE ~ ">0.016"),
         Year = "2022")%>%
  rename(distancetocore = distancetocore22)

valuesboth <- rbind(valuescentres2022,valuescentres2023)
valuesboth$distancetocore <- valuesboth$distancetocore +0.001
valuesboth$D.sum <- valuesboth$D.sum +0.001

valuesboth$Year <- factor(valuesboth$Year, levels = c("2022", "2023"))

glm_model <- glmer(distancetocore~D.sum+(1|Year), data = valuesboth, family = Gamma(link = "log"), control = glmerControl(optimizer = "optimx", optCtrl = list(method = "L-BFGS-B")))

summary(glm_model)

###Predicting values into the dataset so we can plot proper. 
valuesboth$fitted <- predict(glm_model, re.form = NA, type = "response")

predFun <- function(fit) {
  predict(fit, re.form = NA, type = "response")
}

set.seed(123) 
boot_results <- bootMer(glm_model, FUN = predFun, nsim = 100)

if (!is.null(boot_results)) {
  if (any(is.na(boot_results$t))) {
    successful_runs <- complete.cases(boot_results$t)
    filtered_boot_results <- boot_results$t[successful_runs, , drop = FALSE]
  } else {
    filtered_boot_results <- boot_results$t
  }
  
  conf_intervals <- apply(filtered_boot_results, 2, function(x) {
    quantile(x, probs = c(0.025, 0.975))
  })
  valuesboth$lower <- conf_intervals[1, ]
  valuesboth$upper <- conf_intervals[2, ]
} else {
  message("Bootstrapping did not succeed.")
}

####now we plot this
coreandDsum <- ggplot(valuesboth, aes(x = D.sum, y = distancetocore, color = Year)) +
  geom_point(size = 3) +
  geom_line(aes(y = fitted), size = 1, show.legend = FALSE, color = "black") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill="grey",color = NA,show.legend = FALSE) +
  labs(x = "Predicted D.sum of Cell", y = "Mean Distance of Cell to Dingo Core Range Centre") +
  theme_minimal() +
  theme(axis.title = element_text(size = 18),   # Adjust axis title size
        axis.text = element_text(size = 15),    # Adjust axis text size
        axis.text.x = element_text(size = 15, angle = 80, vjust = 0.5),  # Adjust x-axis text size
        axis.text.y = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        title = element_text(size = 18)) +
  scale_color_brewer(palette = "Set1")+
  scale_x_continuous(breaks = seq(0, max(valuesboth$D.sum), by = 0.003))

coreandDsum

png("Figures/CoreandDSum.png", width = 14, height = 10, res= 300, units = "in")

coreandDsum

dev.off()






summaryvalues2023 <- valuescentres2023 %>%
  group_by(D.sum.category)%>%
  summarise(mean_distance = mean(distancetocore, na.rm = TRUE),
            sd_distance = sd(distancetocore,na.rm = TRUE),
            se_distance = sd_distance/ sqrt(n()),
            count = n()
  )%>%
  mutate(Year = '2023')

summaryvalues2022 <- valuescentres2022 %>%
  group_by(D.sum.category)%>%
  summarise(mean_distance = mean(distancetocore, na.rm = TRUE),
            sd_distance = sd(distancetocore,na.rm = TRUE),
            se_distance = sd_distance/ sqrt(n()),
            count = n()
  )%>%
  mutate(Year = '2022')

summariesboth<- rbind(summaryvalues2022,summaryvalues2023)

summariesboth$D.sum.category <- factor(summariesboth$D.sum.category, levels = c("0-0.002", "0.002-0.004","0.004-0.006","0.006-0.008",">0.008"))




#assign some factors for ordering 

valuescentres2022$D.sum.category <- factor(valuescentres2022$D.sum.category, levels = c("0-0.002", "0.002-0.004","0.004-0.006","0.006-0.008","0.008-0.010","0.010-0.012","0.012-0.014","0.014-0.016",">0.016"))

valuescentres2023$D.sum.category <- factor(valuescentres2023$D.sum.category, levels = c("0-0.002", "0.002-0.004","0.004-0.006","0.006-0.008","0.008-0.010","0.010-0.012","0.012-0.014","0.014-0.016",">0.016"))

valuescentres2022$Zone <- factor(valuescentres2022$Zone, levels = c("None", "Periph","Core"))

valuescentres2023$Zone <- factor(valuescentres2023$Zone, levels = c("None", "Periph","Core"))





###Analysis of cameras and individuals.  

#lets first take the camera closest to each 'peak'. 



#2022:
#peak 1 - PS1
#Peak 2 - PS2
#Peak 3 - PS6
#Peak 4 - PS9
#Peak 5 - PS21
#Peak 6 - PS20 

#2023: 
#Peak 1 - PS1
#Peak 2 - PS2
#Peak 3 - PS6
#Peak 4 - PS11
#Peak 5 - PS21


#Now we are going to find the individuals identified at each cam, and see who's there most. 
#First 22

DetsEachCam22 <- dingodoubleids2022 %>%
  filter(Trap == 'PS1' |
         Trap == 'PS2' |
         Trap == 'PS6' |
         Trap == 'PS11' |
         Trap == 'PS21' |
         Trap == 'PS20')%>%
  arrange(Trap, DateTime)%>%
  mutate(Pack = case_when(
    Trap == "PS1"~"JB",
    Trap == "PS2"~"WT",
    Trap == "PS6"~"SL",
    Trap == "PS11"~"MB",
    Trap == "PS21"~"YG",
    Trap == "PS20"~"SB"
  ))%>%
  group_by(Pack)%>%
  summarise(CountPopSurvey = n_distinct(Individual),
            IndividualPopSurvey = paste(unique(Individual), collapse = ", "))


###-------------2022

groupevents22 <- dingodoubleids2022 %>%
  filter(Trap == 'PS1' |
           Trap == 'PS2' |
           Trap == 'PS6' |
           Trap == 'PS11' |
           Trap == 'PS21' |
           Trap == 'PS20')%>%
  group_by(DateTime, Trap) %>%
  summarise(Individuals = list(Individual), .groups = 'drop')

groupevents22 <- groupevents22 %>%
  mutate(Individuals = map(Individuals, ~as.character(.x))) %>%
  unnest_wider(Individuals, names_sep = "Individual_")%>%
  filter(!is.na(IndividualsIndividual_2)) %>%
  filter(!apply(.[, c("IndividualsIndividual_1", "IndividualsIndividual_2")], 1, function(row) any(grepl('21', row))))%>%
  mutate(Event = row_number())

summarypairsstats22 <- groupevents22 %>%
  group_by(Trap)%>%
  summarise(count = n())

#Look at this dataframe, and see opposite sex pairs who was seen at each camera

#PS1 - 1 adult pair, UOM2008 and UOF1501 6 times. 
#PS2 - 1 adult pair, UOF1906 and UOM1707 4 times.
#PS6 - 1 adult pair, SLF1501 and PCM1601 2 times.
#PS9 - 2 adult pairs, UOF1801 and UOM1701 4 times, UOF1801 and UOM2002 1 time.
#PS20 - 1 adult pair, UOF2013 and UOM2012 1 time. 
#PS21 - 1 adult pair, UOM2007 and MBF1701 1 time. 





###-------------2023


DetsEachCam23 <- dingodoubleids2023 %>%
  filter(Trap == 'PS1' |
           Trap == 'PS2' |
           Trap == 'PS6' |
           Trap == 'PS11' |
           Trap == 'PS21')%>%
  arrange(Trap, DateTime)%>%
  mutate(Pack = case_when(
    Trap == "PS1"~"JB",
    Trap == "PS2"~"WT",
    Trap == "PS6"~"SL",
    Trap == "PS11"~"MB",
    Trap == "PS21"~"YG"
  ))%>%
  group_by(Pack)%>%
  summarise(CountPopSurvey = n_distinct(Individual),
            IndividualPopSurvey = paste(unique(Individual), collapse = ", "))



groupevents23 <- dingodoubleids2023 %>%
  filter(Trap == 'PS1' |
           Trap == 'PS2' |
           Trap == 'PS6' |
           Trap == 'PS11' |
           Trap == 'PS21')%>%
  group_by(DateTime, Trap) %>%
  summarise(Individuals = list(Individual), .groups = 'drop')

groupevents23 <- groupevents23 %>%
  mutate(Individuals = map(Individuals, ~as.character(.x))) %>%
  unnest_wider(Individuals, names_sep = "Individual_")%>%
  filter(!is.na(IndividualsIndividual_2)) %>%
  filter(!apply(.[, c("IndividualsIndividual_1", "IndividualsIndividual_2")], 1, function(row) any(grepl('22', row))))%>%
  mutate(Event = row_number())

summarypairsstats23 <- groupevents23 %>%
  group_by(Trap)%>%
  summarise(count = n())

#Look at these dataframe, and see opposite sex pairs who were seen at each camera


#PS1 - 1 adult pair, UOF1709 and UOM2004 6 times. 
#PS2 - 1 adult pair, UOF1906 and UOM1707 3 times.
#PS6 - None.-USE SLF1501 and UOM2002. 
#PS11 - 1 adult pairs, UOF1801 and UOM2001 6 times
#PS21 - 1 adult pair, UOM2007 and MBF1701 4 times. 



###Now for 2022 and 2023, lets get the cameras each of these individuals was at-we'll use the number of detections as a measure of quantity, then plot. 

####Reminder for tomorrow- get individuals from each camera, and make points of where they are. Could even do kdes? 


detsindividuals2022 <- dingodoubleids2022%>%
  group_by(Trap, Individual)%>%
  summarise(Count = n())%>%
  filter(Individual == 'UOM2008'|
         Individual == 'UOF1501'|
         Individual == 'UOF1906'|
         Individual == 'UOM1707'|
           Individual == 'SLF1501'|
           Individual == 'PCM1601'|
           Individual == 'UOM1701'|
           Individual == 'UOF1801'|
           Individual == 'UOF2013'|
           Individual == 'UOM2012'|
           Individual == 'UOM2007'|
           Individual == 'MBF1701')%>%
  group_by(Trap)%>%
  mutate(Pack = case_when(Individual == 'UOM2008' ~ "JB",
                          Individual == 'UOF1501' ~ "JB",
                          Individual == 'UOF1906' ~ "WT",
                          Individual == 'UOM1707' ~ "WT",
                          Individual == 'SLF1501' ~ "SL",
                          Individual == 'PCM1601' ~ "SL",
                          Individual == 'UOM1701' ~ "MB",
                          Individual == 'UOF1801' ~ "MB",
                          Individual == 'UOF2013' ~ "SB",
                          Individual == 'UOM2012' ~ "SB",
                          Individual == 'UOM2007' ~ "YG",
                          Individual == 'MBF1701' ~ "YG",
                          Individual == TRUE ~ NA))

summarydf22countsperpack <- detsindividuals2022 %>%
  group_by(Trap, Pack)%>%
  summarise(Dets = sum(Count))

##2023



detsindividuals2023 <- dingodoubleids2023%>%
  group_by(Trap, Individual)%>%
  summarise(Count = n())%>%
  filter(Individual == 'UOF1709'|
           Individual == 'UOM2004'|
           Individual == 'UOF1906'|
           Individual == 'UOM1707'|
           Individual == 'SLF1501'|
           Individual == 'UOM2002'|
           Individual == 'UOM2001'|
           Individual == 'UOF1801'|
           Individual == 'UOM2007'|
           Individual == 'MBF1701')%>%
  group_by(Trap)%>%
  mutate(Pack = case_when(Individual == 'UOF1709' ~ "JB",
                          Individual == 'UOM2004' ~ "JB",
                          Individual == 'UOF1906' ~ "WT",
                          Individual == 'UOM1707' ~ "WT",
                          Individual == 'SLF1501' ~ "SL",
                          Individual == 'UOM2002' ~ "SL",
                          Individual == 'UOM2001' ~ "MB",
                          Individual == 'UOF1801' ~ "MB",
                          Individual == 'UOM2007' ~ "YG",
                          Individual == 'MBF1701' ~ "YG",
                          Individual == TRUE ~ NA))

summarydf23countsperpack <- detsindividuals2023 %>%
  group_by(Trap, Pack)%>%
  summarise(Dets = sum(Count))

####add in our coordinates, and create an sf object with the correct weights for each


summarydf22countsperpack <- summarydf22countsperpack %>%
  left_join(trapsallPS2022, by = "Trap")

summarydf23countsperpack <- summarydf23countsperpack %>%
  left_join(trapsallPS2023, by = "Trap")


Weight2022 <- st_as_sf(summarydf22countsperpack, coords = c("x", "y"),crs = st_crs(32756)) 

Weight2023 <- st_as_sf(summarydf23countsperpack, coords = c("x", "y"), crs = st_crs(32756)) 

####Seperate into  packs manually. 

Weight22JB <- Weight2022 %>%
  filter(Pack == 'JB')

Weight22WT <- Weight2022 %>%
  filter(Pack == 'WT')

Weight22SL <- Weight2022 %>%
  filter(Pack == 'SL')

Weight22MB <- Weight2022 %>%
  filter(Pack == 'MB')

Weight22YG <- Weight2022 %>%
  filter(Pack == 'YG')

Weight22SB <- Weight2022 %>%
  filter(Pack == 'SB')

###now do 23
Weight23JB <- Weight2023 %>%
  filter(Pack == 'JB')

Weight23WT <- Weight2023 %>%
  filter(Pack == 'WT')

Weight23SL <- Weight2023 %>%
  filter(Pack == 'SL')

Weight23MB <- Weight2023 %>%
  filter(Pack == 'MB')

Weight23YG <- Weight2023 %>%
  filter(Pack == 'YG')


####Calculate the KDE weights for each year and pack

cell_size <- 500
band_width <-3000 #Max dist between two cameras
band_width_core <- 3000

valuescentres2022coords <- st_as_sf(maskfinalpoints, coords = c("x","y"),crs = st_crs(32756))


# fit kde's - first 6, 2022. 


kde_2022JB <- Weight22JB %>%
  kde(band_width = band_width, kernel = "quartic", grid = valuescentres2022coords, weights = Weight22JB$Dets, scaled = FALSE)%>%
  mutate(Year = '2022',
         CamPack = 'JB')

kde_2022WT <- Weight22WT %>%
  kde(band_width = band_width, kernel = "quartic", grid = valuescentres2022coords, weights = Weight22WT$Dets, scaled = FALSE)%>%
  mutate(Year = '2022',
         CamPack = 'WT')

kde_2022SL <- Weight22SL %>%
  kde(band_width = band_width, kernel = "quartic", grid = valuescentres2022coords, weights = Weight22SL$Dets, scaled = FALSE)%>%
  mutate(Year = '2022',
         CamPack = 'SL')

kde_2022MB <- Weight22MB %>%
  kde(band_width = band_width, kernel = "quartic", grid = valuescentres2022coords, weights = Weight22MB$Dets, scaled = FALSE)%>%
  mutate(Year = '2022',
         CamPack = 'MB')

kde_2022YG <- Weight22YG %>%
  kde(band_width = band_width, kernel = "quartic", grid = valuescentres2022coords, weights = Weight22YG$Dets, scaled = FALSE)%>%
  mutate(Year = '2022',
         CamPack = 'YG')

kde_2022SB <- Weight22SB %>%
  kde(band_width = band_width, kernel = "quartic", grid = valuescentres2022coords, weights = Weight22SB$Dets, scaled = FALSE)%>%
  mutate(Year = '2022',
         CamPack = 'SB')

# fit kde's - first 5, 2023. 
kde_2023JB <- Weight23JB %>%
  kde(band_width = band_width, kernel = "quartic", grid = valuescentres2022coords, weights = Weight23JB$Dets, scaled = FALSE)%>%
  mutate(Year = '2023',
         CamPack = 'JB')

kde_2023WT <- Weight23WT %>%
  kde(band_width = band_width, kernel = "quartic", grid = valuescentres2022coords, weights = Weight23WT$Dets, scaled = FALSE)%>%
  mutate(Year = '2023',
         CamPack = 'WT')

kde_2023SL <- Weight23SL %>%
  kde(band_width = band_width, kernel = "quartic", grid = valuescentres2022coords, weights = Weight23SL$Dets, scaled = FALSE)%>%
  mutate(Year = '2023',
         CamPack = 'SL')

kde_2023MB <- Weight23MB %>%
  kde(band_width = band_width, kernel = "quartic", grid = valuescentres2022coords, weights = Weight23MB$Dets, scaled = FALSE)%>%
  mutate(Year = '2023',
         CamPack = 'MB')

kde_2023YG <- Weight23YG %>%
  kde(band_width = band_width, kernel = "quartic", grid = valuescentres2022coords, weights = Weight23YG$Dets, scaled = FALSE)%>%
  mutate(Year = '2023',
         CamPack = 'YG')

##########Repeat for cores


kde_2022_CoreJB <- Weight22JB %>%
  kde(band_width = band_width_core, kernel = "quartic", grid = valuescentres2022coords, weights = Weight22JB$Dets, scaled = FALSE)%>%
  mutate(Year = '2022',
         CamPack = 'JB')

kde_2022_CoreWT <- Weight22WT %>%
  kde(band_width = band_width_core, kernel = "quartic", grid = valuescentres2022coords, weights = Weight22WT$Dets, scaled = FALSE)%>%
  mutate(Year = '2022',
         CamPack = 'WT')

kde_2022_CoreSL <- Weight22SL %>%
  kde(band_width = band_width_core, kernel = "quartic", grid = valuescentres2022coords, weights = Weight22SL$Dets, scaled = FALSE)%>%
  mutate(Year = '2022',
         CamPack = 'SL')

kde_2022_CoreMB <- Weight22MB %>%
  kde(band_width = band_width_core, kernel = "quartic", grid = valuescentres2022coords, weights = Weight22MB$Dets, scaled = FALSE)%>%
  mutate(Year = '2022',
         CamPack = 'MB')

kde_2022_CoreYG <- Weight22YG %>%
  kde(band_width = band_width_core, kernel = "quartic", grid = valuescentres2022coords, weights = Weight22YG$Dets, scaled = FALSE)%>%
  mutate(Year = '2022',
         CamPack = 'YG')

kde_2022_CoreSB <- Weight22SB %>%
  kde(band_width = band_width_core, kernel = "quartic", grid = valuescentres2022coords, weights = Weight22SB$Dets, scaled = FALSE)%>%
  mutate(Year = '2022',
         CamPack = 'SB')

# fit kde's - first 5, 2023. 
kde_2023_CoreJB <- Weight23JB %>%
  kde(band_width = band_width_core, kernel = "quartic", grid = valuescentres2022coords, weights = Weight23JB$Dets, scaled = FALSE)%>%
  mutate(Year = '2023',
         CamPack = 'JB')

kde_2023_CoreWT <- Weight23WT %>%
  kde(band_width = band_width_core, kernel = "quartic", grid = valuescentres2022coords, weights = Weight23WT$Dets, scaled = FALSE)%>%
  mutate(Year = '2023',
         CamPack = 'WT')

kde_2023_CoreSL <- Weight23SL %>%
  kde(band_width = band_width_core, kernel = "quartic", grid = valuescentres2022coords, weights = Weight23SL$Dets, scaled = FALSE)%>%
  mutate(Year = '2023',
         CamPack = 'SL')

kde_2023_CoreMB <- Weight23MB %>%
  kde(band_width = band_width_core, kernel = "quartic", grid = valuescentres2022coords, weights = Weight23MB$Dets, scaled = FALSE)%>%
  mutate(Year = '2023',
         CamPack = 'MB')

kde_2023_CoreYG <- Weight23YG %>%
  kde(band_width = band_width_core, kernel = "quartic", grid = valuescentres2022coords, weights = Weight23YG$Dets, scaled = FALSE)%>%
  mutate(Year = '2023',
         CamPack = 'YG')





###plot 22 and 23


####experimenting with doing all seperately: 



joined_data22 <- st_join(valuescentres2022coords, AllAllsf22kde, join = st_within)

joined_data22 <- joined_data22 %>%
  dplyr::select(CollarPack,TerritoryZone,geometry)

joined_data22<- joined_data22 %>%
  group_by(geometry) %>%
  arrange(desc(CollarPack == 'SL')) %>% # Prioritize SL values
  slice(1) %>% # Keep the first row per group (after sorting)
  ungroup()

joined_data22 <- joined_data22 %>%
  mutate(CollarPack= case_when(
    is.na(CollarPack)~ 'None',
    TRUE ~ as.character(CollarPack)
  ))%>%
  mutate(TerritoryZone= case_when(
    is.na(TerritoryZone)~ 'None',
    TRUE ~ as.character(TerritoryZone)
  ))


joined_data23 <- st_join(valuescentres2022coords, AllAllKdesf23, join = st_within)

joined_data23 <- joined_data23 %>%
  dplyr::select(CollarPack,TerritoryZone,geometry)

joined_data23<- joined_data23 %>%
  group_by(geometry) %>%
  arrange(desc(CollarPack == 'MB')) %>% # Prioritize MB values
  slice(1) %>% # Keep the first row per group (after sorting)
  ungroup()

joined_data23 <- joined_data23 %>%
  mutate(CollarPack= case_when(
    is.na(CollarPack)~ 'None',
    TRUE ~ as.character(CollarPack)
  ))%>%
  mutate(TerritoryZone= case_when(
    is.na(TerritoryZone)~ 'None',
    TRUE ~ as.character(TerritoryZone)
  ))


###now we have all the DFs close to together. - lets put values of d sum, and collar data together. 

valuescentres2022parts <- st_join(valuescentres2022coords,joined_data22,by = 'geometry')

valuescentres2022$TerritoryZoneCollar <- valuescentres2022parts$TerritoryZone

valuescentres2022$CollarPack <- valuescentres2022parts$CollarPack

#and 2023
valuescentres2023parts <- st_join(valuescentres2022coords,joined_data23,by = 'geometry')

valuescentres2023$TerritoryZoneCollar <- valuescentres2023parts$TerritoryZone

valuescentres2023$CollarPack <- valuescentres2023parts$CollarPack

#remove old zone
valuescentres2022 <- valuescentres2022 %>%
  dplyr::select(!Zone)

valuescentres2023 <- valuescentres2023 %>%
  dplyr::select(!Zone)


####now next step is to get them together-rbind.


long22kde <- rbind(kde_2022JB,kde_2022WT,kde_2022SL,kde_2022MB,kde_2022YG,kde_2022SB)

long23kde <- rbind(kde_2023JB,kde_2023WT,kde_2023SL,kde_2023MB,kde_2023YG)

long22kdecore <- rbind(kde_2022_CoreJB,kde_2022_CoreWT,kde_2022_CoreSL,kde_2022_CoreMB,kde_2022_CoreYG,kde_2022_CoreSB)

long23kdecore <- rbind(kde_2023_CoreJB,kde_2023_CoreWT,kde_2023_CoreSL,kde_2023_CoreMB,kde_2023_CoreYG)


#get outta sf 

long22kde <- long22kde %>%
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(long22kde %>% dplyr::select(kde_value, Year, CamPack))

long23kde <- long23kde %>%
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(long23kde %>% dplyr::select(kde_value, Year, CamPack))

long22kdecore <- long22kdecore %>%
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(long22kdecore %>% dplyr::select(kde_value, Year, CamPack))

long23kdecore <- long23kdecore %>%
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(long23kdecore %>% dplyr::select(kde_value, Year, CamPack))



###remove duplicate rows by selecting highest value at each point.

long22kde<- long22kde %>%
  group_by(geometry)%>%
  arrange(desc(kde_value))%>%
  slice(1)%>%
  ungroup()%>%
  mutate(TerritoryZoneCam = case_when(kde_value <= 1 ~ "None",
    TRUE ~ "Periph"),
         CamPack = case_when(TerritoryZoneCam == "None"~ "None",
                             TRUE ~CamPack))

###remove duplicate rows

long23kde<- long23kde %>%
  group_by(geometry)%>%
  arrange(desc(kde_value))%>%
  slice(1)%>%
  ungroup()%>%
  mutate(TerritoryZoneCam = case_when(kde_value <= 1 ~ "None",
                                      TRUE ~ "Periph"),
         CamPack = case_when(TerritoryZoneCam == "None"~ "None",
                             TRUE ~CamPack))

#repeat for core, and add in a value called 'periph' for points that have kde_value <0.002

long22kdecore<- long22kdecore %>%
  group_by(geometry)%>%
  arrange(desc(kde_value))%>%
  slice(1)%>%
  ungroup()%>%
  mutate(TerritoryZoneCam = case_when(kde_value <= 0.002 ~ "Periph",
                          TRUE ~ "Core"
                          ))

###remove duplicate rows

long23kdecore<- long23kdecore %>%
  group_by(geometry)%>%
  arrange(desc(kde_value))%>%
  slice(1)%>%
  ungroup()%>%
  mutate(TerritoryZoneCam = case_when(kde_value <= 0.002 ~ "Periph",
                          TRUE ~ "Core"
  ))


###Finally, lets combine this into one df, with core taking precedence. 

combined22cams <- long22kde%>%
  mutate(across(everything(), ~ if_else(TerritoryZoneCam == "Core", ., long22kde[[cur_column()]])))


combined23cams <- long23kde%>%
  mutate(across(everything(), ~ if_else(TerritoryZoneCam == "Core", ., long23kde[[cur_column()]])))






#plot
ggplot(combined22cams) +
  geom_point(aes(x = X, y = Y, size = kde_value, color = CamPack)) +
  scale_size(range = c(1, 5), name = "KDE Value")+ # Customize colors for CamPack
  labs(x = "X Coordinate", y = "Y Coordinate") +
  theme_minimal()

ggplot(combined23cams) +
  geom_point(aes(x = X, y = Y, size = kde_value, color = CamPack)) +
  scale_size(range = c(1, 5), name = "KDE Value")+ # Customize colors for CamPack
  labs(x = "X Coordinate", y = "Y Coordinate") +
  theme_minimal()

ggplot(long22kdecore) +
  geom_point(aes(x = X, y = Y, size = kde_value, color = CamPack)) +
  scale_size(range = c(1, 5), name = "KDE Value")+ # Customize colors for CamPack
  labs(x = "X Coordinate", y = "Y Coordinate") +
  theme_minimal()

ggplot(long23kdecore) +
  geom_point(aes(x = X, y = Y, size = kde_value, color = CamPack)) +
  scale_size(range = c(1, 5), name = "KDE Value")+ # Customize colors for CamPack
  labs(x = "X Coordinate", y = "Y Coordinate") +
  theme_minimal()

####Looks goose!!!!

####Lets combine the three datasets:
valuescentres2022$CamPack <- long22kde$CamPack
valuescentres2022$kde_value <- long22kde$kde_value
valuescentres2022$TerritoryZoneCam <- long22kde$TerritoryZoneCam


valuescentres2023$CamPack <- long23kde$CamPack
valuescentres2023$kde_value <- long23kde$kde_value
valuescentres2023$TerritoryZoneCam <- long23kde$TerritoryZoneCam


####Now lets compare the KUDs to the other kde lol. 

valuescentres2022$TerritoryZoneCam <- factor(valuescentres2022$TerritoryZoneCam, levels = c("None", "Periph","Core"))

valuescentres2022$TerritoryZoneCollar <- factor(valuescentres2022$TerritoryZoneCollar, levels = c("None", "Periph","Core"))

valuescentres2023$TerritoryZoneCam <- factor(valuescentres2023$TerritoryZoneCam, levels = c("None", "Periph","Core"))

valuescentres2023$TerritoryZoneCollar <- factor(valuescentres2023$TerritoryZoneCollar, levels = c("None", "Periph","Core"))



##Calculating overlap and size. We'll use concaveman to generate a concave hull around the points, for the camera trap HRs. then we can compare to the collars.  

library(concaveman)

#2022 again- filter and transform to sf. 
jb22kdesampcam <- valuescentres2022%>%
  filter(CamPack == "JB")
jb22kdesampcam <- st_as_sf(jb22kdesampcam, coords = c("x","y"), crs = 32756)

wt22kdesampcam <- valuescentres2022%>%
  filter(CamPack == "WT")
wt22kdesampcam <- st_as_sf(wt22kdesampcam, coords = c("x","y"), crs = 32756)


sl22kdesampcam <- valuescentres2022%>%
  filter(CamPack == "SL")
sl22kdesampcam <- st_as_sf(sl22kdesampcam, coords = c("x","y"), crs = 32756)

mb22kdesampcam <- valuescentres2022%>%
  filter(CamPack == "MB")
mb22kdesampcam <- st_as_sf(mb22kdesampcam, coords = c("x","y"), crs = 32756)

yg22kdesampcam <- valuescentres2022%>%
  filter(CamPack == "YG")
yg22kdesampcam <- st_as_sf(yg22kdesampcam, coords = c("x","y"), crs = 32756)


#2023 again- filter and transform to sf. 
jb23kdesampcam <- valuescentres2023%>%
  filter(CamPack == "JB")
jb23kdesampcam <- st_as_sf(jb23kdesampcam, coords = c("x","y"), crs = 32756)

wt23kdesampcam <- valuescentres2023%>%
  filter(CamPack == "WT")
wt23kdesampcam <- st_as_sf(wt23kdesampcam, coords = c("x","y"), crs = 32756)


sl23kdesampcam <- valuescentres2023%>%
  filter(CamPack == "SL")
sl23kdesampcam <- st_as_sf(sl23kdesampcam, coords = c("x","y"), crs = 32756)

mb23kdesampcam <- valuescentres2023%>%
  filter(CamPack == "MB")
mb23kdesampcam <- st_as_sf(mb23kdesampcam, coords = c("x","y"), crs = 32756)

yg23kdesampcam <- valuescentres2023%>%
  filter(CamPack == "YG")
yg23kdesampcam <- st_as_sf(yg23kdesampcam, coords = c("x","y"), crs = 32756)


#####We'll also have to do for all of the collar home ranges too. just get into sf 
study_area_clippedcrs <- st_transform(study_area_clipped, crs = 32756)

JB22PeriphKDEsf <- st_as_sf(Andy21PeriphKDE, crs = st_crs(32756))
JB22PeriphKDEsf <- st_transform(JB22PeriphKDEsf, crs = 32756)
JB22PeriphClip <- st_intersection(JB22PeriphKDEsf, study_area_clippedcrs)

WT22PeriphKDEsf <- st_as_sf(SWY21PeriphKDE, crs = st_crs(32756))
WT22PeriphKDEsf <- st_transform(WT22PeriphKDEsf, crs = 32756)
WT22PeriphClip <- st_intersection(WT22PeriphKDEsf, study_area_clippedcrs)

SL22PeriphKDEsf <- st_as_sf(Cathy21PeriphKDE, crs = st_crs(32756))
SL22PeriphKDEsf <- st_transform(SL22PeriphKDEsf, crs = 32756)
SL22PeriphClip <- st_intersection(SL22PeriphKDEsf, study_area_clippedcrs)

MB22PeriphKDEsf <- st_as_sf(RD21PeriphKDE, crs = st_crs(32756))
MB22PeriphKDEsf <- st_transform(MB22PeriphKDEsf, crs = 32756)
MB22PeriphClip <- st_intersection(MB22PeriphKDEsf, study_area_clippedcrs)

YG22PeriphKDEsf <- st_as_sf(Tekka21PeriphKDE, crs = st_crs(32756))
YG22PeriphKDEsf <- st_transform(YG22PeriphKDEsf, crs = 32756)
YG22PeriphClip <- st_intersection(YG22PeriphKDEsf, study_area_clippedcrs)

JB23PeriphKDEsf <- st_as_sf(Andy23PeriphKDE, crs = st_crs(32756))
JB23PeriphKDEsf <- st_transform(JB23PeriphKDEsf, crs = 32756)
JB23PeriphClip <- st_intersection(JB23PeriphKDEsf, study_area_clippedcrs)

WT23PeriphKDEsf <- st_as_sf(SWY23PeriphKDE, crs = st_crs(32756))
WT23PeriphKDEsf <- st_transform(WT23PeriphKDEsf, crs = 32756)
WT23PeriphClip <- st_intersection(WT23PeriphKDEsf, study_area_clippedcrs)

SL23PeriphKDEsf <- st_as_sf(RD23PeriphKDE, crs = st_crs(32756))
SL23PeriphKDEsf <- st_transform(SL23PeriphKDEsf, crs = 32756)
SL23PeriphClip <- st_intersection(SL23PeriphKDEsf, study_area_clippedcrs)

MB23PeriphKDEsf <- st_as_sf(Bombah23PeriphKDE, crs = st_crs(32756))
MB23PeriphKDEsf <- st_transform(MB23PeriphKDEsf, crs = 32756)
MB23PeriphClip <- st_intersection(MB23PeriphKDEsf, study_area_clippedcrs)

YG23PeriphKDEsf <- st_as_sf(Tekka23PeriphKDE, crs = st_crs(32756))
YG23PeriphKDEsf <- st_transform(YG23PeriphKDEsf, crs = 32756)
YG23PeriphClip <- st_intersection(YG23PeriphKDEsf, study_area_clippedcrs)


JB22polycam <-concaveman(jb22kdesampcam,concavity = 2)
WT22polycam <-concaveman(wt22kdesampcam,concavity = 2)
SL22polycam <-concaveman(sl22kdesampcam,concavity = 2)
MB22polycam <-concaveman(mb22kdesampcam,concavity = 2)
YG22polycam <-concaveman(yg22kdesampcam,concavity = 2)

JB23polycam <-concaveman(jb23kdesampcam,concavity = 2)
WT23polycam <-concaveman(wt23kdesampcam,concavity = 2)
SL23polycam <-concaveman(sl23kdesampcam,concavity = 2)
MB23polycam <-concaveman(mb23kdesampcam,concavity = 2)
YG23polycam <-concaveman(yg23kdesampcam,concavity = 2)


#calculate areas
JB22collar <- st_area(JB22PeriphClip)
WT22collar <- st_area(WT22PeriphClip)
SL22collar <- st_area(SL22PeriphClip)
MB22collar <- st_area(MB22PeriphClip)
YG22collar <- st_area(YG22PeriphClip)

JB22cam <- st_area(JB22polycam)
WT22cam <- st_area(WT22polycam)
SL22cam <- st_area(SL22polycam)
MB22cam <- st_area(MB22polycam)
YG22cam <- st_area(YG22polycam)

#calculate areas
JB23collar <- st_area(JB23PeriphClip)
WT23collar <- st_area(WT23PeriphClip)
SL23collar <- st_area(SL23PeriphClip)
MB23collar <- st_area(MB23PeriphClip)
YG23collar <- st_area(YG23PeriphClip)

JB23cam <- st_area(JB23polycam)
WT23cam <- st_area(WT23polycam)
SL23cam <- st_area(SL23polycam)
MB23cam <- st_area(MB23polycam)
YG23cam <- st_area(YG23polycam)

#now calculate intersections

#2022
JBint22 <- st_intersection(JB22PeriphClip,JB22polycam)
WTint22 <- st_intersection(WT22PeriphClip,WT22polycam)
SLint22 <- st_intersection(SL22PeriphClip,SL22polycam)
MBint22 <- st_intersection(MB22PeriphClip,MB22polycam)
YGint22 <- st_intersection(YG22PeriphClip,YG22polycam)

#collar
percentJB22 <- (sum(st_area(JBint22)) /sum(JB22collar) )*100
percentWT22 <- (sum(st_area(WTint22)) /sum(WT22collar) )*100
percentSL22 <- (sum(st_area(SLint22)) /sum(SL22collar) )*100
percentMB22 <- (sum(st_area(MBint22)) /sum(MB22collar) )*100
percentYG22 <- (sum(st_area(YGint22)) /sum(YG22collar) )*100

#cam
percentJB22cam <- (sum(st_area(JBint22)) /sum(JB22cam) )*100
percentWT22cam <- (sum(st_area(WTint22)) /sum(WT22cam) )*100
percentSL22cam <- (sum(st_area(SLint22)) /sum(SL22cam) )*100
percentMB22cam <- (sum(st_area(MBint22)) /sum(MB22cam) )*100
percentYG22cam <- (sum(st_area(YGint22)) /sum(YG22cam) )*100


#2023
JBint23 <- st_intersection(JB23PeriphClip,JB23polycam)
WTint23 <- st_intersection(WT23PeriphClip,WT23polycam)
SLint23 <- st_intersection(SL23PeriphClip,SL23polycam)
MBint23 <- st_intersection(MB23PeriphClip,MB23polycam)
YGint23 <- st_intersection(YG23PeriphClip,YG23polycam)

#collar
percentJB23 <- (sum(st_area(JBint23)) /sum(JB23collar) )*100
percentWT23 <- (sum(st_area(WTint23)) /sum(WT23collar) )*100
percentSL23 <- (sum(st_area(SLint23)) /sum(SL23collar) )*100
percentMB23 <- (sum(st_area(MBint23)) /sum(MB23collar) )*100
percentYG23 <- (sum(st_area(YGint23)) /sum(YG23collar) )*100

#cam
percentJB23cam <- (sum(st_area(JBint23)) /sum(JB23cam) )*100
percentWT23cam <- (sum(st_area(WTint23)) /sum(WT23cam) )*100
percentSL23cam <- (sum(st_area(SLint23)) /sum(SL23cam) )*100
percentMB23cam <- (sum(st_area(MBint23)) /sum(MB23cam) )*100
percentYG23cam <- (sum(st_area(YGint23)) /sum(YG23cam) )*100

collar_percentages <- c(percentJB22, percentWT22, percentSL22, percentMB22, percentYG22
,percentJB23, percentWT23, percentSL23, percentMB23, percentYG23)

cam_percentages <- c(percentJB22cam, percentWT22cam, percentSL22cam, percentMB22cam, percentYG22cam
,percentJB23cam, percentWT23cam, percentSL23cam, percentMB23cam, percentYG23cam)

collarsize <- c(JB22collar, WT22collar,SL22collar,MB22collar,YG22collar,JB23collar,WT23collar,SL23collar,MB23collar,YG23collar)

camsize <- c(JB22cam, WT22cam,SL22cam,MB22cam,YG22cam,JB23cam,WT23cam,SL23cam,MB23cam,YG23cam)


# Create a vector of names for each row
locations <- c("JB22", "WT22", "SL22", "MB22", "YG22","JB23", "WT23", "SL23", "MB23", "YG23")

# Combine into a data frame
percent_df <- data.frame(
  Location = locations,
  Collar_Percentage = collar_percentages,
  Collarsize= collarsize,
  Camera_Percentage = cam_percentages,
  Camsize = camsize
)



#Figure exploring different values of concavity. ---supplementary materials.

#We'll explore looking at MB pack 

#get coord points lower values. 
valuesmb22pointsfig <- st_intersection(valuescentres2022coords, MB22PeriphClip)

MB22polycam0point5 <-concaveman(mb22kdesampcam,concavity = 0.5)
MB22polycam1 <-concaveman(mb22kdesampcam,concavity = 1)
MB22polycam2 <-concaveman(mb22kdesampcam,concavity = 2)
MB22polycam5 <-concaveman(mb22kdesampcam,concavity = 5)
MB22polycam20 <-concaveman(mb22kdesampcam,concavity = 20)



pointfive <- ggplot() + 
  geom_sf(data = MB22polycam0point5, aes(fill = "Camera Home Range"), color = NA) +  
  geom_sf(data = valuesmb22pointsfig, aes(size = "Pixels"), color = "black") + 
  scale_fill_manual(
    name = "",  
    values = c("Camera Home Range" = "pink"),  # Fill colors
    labels = c("Camera home\nrange minimum\nconcave hull")  # Custom labels
  ) +
  scale_size_manual(
    name = "",  # Legend title
    values = c("Pixels" = 0.5),  # Size for habitat mask points
    labels = c("Pixels in\nhabitat mask")  # Custom label for the points
  ) +
  theme_minimal()+
  labs(title = "Concavity = 0.5") +
  theme(
    legend.text = element_text(size = 14),  # Increase legend text size
    legend.title = element_text(size = 16),
    title = element_text(size = 20)
  )

one <- ggplot() + 
  geom_sf(data = MB22polycam1, aes(fill = "Camera Home Range"), color = NA) +  # Camera home range
  geom_sf(data = valuesmb22pointsfig, aes(size = "Pixels"), color = "black") +  # Habitat mask points
  scale_fill_manual(
    name = "",  # Legend title
    values = c("Camera Home Range" = "pink"),  # Fill colors
    labels = c("Camera home\nrange minimum\nconcave hull")  # Custom labels
  ) +
  scale_size_manual(
    name = "",  # Legend title
    values = c("Pixels" = 0.5),  # Size for habitat mask points
    labels = c("Pixels in\nhabitat mask")  # Custom label for the points
  )+
  theme_minimal()+
  labs(title = "Concavity = 1") +
  theme(
    legend.text = element_text(size = 14),  # Increase legend text size
    legend.title = element_text(size = 16),
    title = element_text(size = 20)
  ) 

two <- ggplot() + 
  geom_sf(data = MB22polycam2, aes(fill = "Camera Home Range"), color = NA) +  # Camera home range
  geom_sf(data = valuesmb22pointsfig, aes(size = "Pixels"), color = "black") +  # Habitat mask points
  scale_fill_manual(
    name = "",  # Legend title
    values = c("Camera Home Range" = "pink"),  # Fill colors
    labels = c("Camera home\nrange minimum\nconcave hull")  # Custom labels
  ) +
  scale_size_manual(
    name = "",  # Legend title
    values = c("Pixels" = 0.5),  # Size for habitat mask points
    labels = c("Pixels in\nhabitat mask")  # Custom label for the points
  ) +
  theme_minimal()+
  labs(title = "Concavity = 2")+
  theme(
    legend.text = element_text(size = 14),  # Increase legend text size
    legend.title = element_text(size = 16),
    title = element_text(size = 20)
  ) 

five <- ggplot() + 
  geom_sf(data = MB22polycam5, aes(fill = "Camera Home Range"), color = NA) +  # Camera home range
  geom_sf(data = valuesmb22pointsfig, aes(size = "Pixels"), color = "black") +  # Habitat mask points
  scale_fill_manual(
    name = "",  # Legend title
    values = c("Camera Home Range" = "pink"),  # Fill colors
    labels = c("Camera home\nrange minimum\nconcave hull")  # Custom labels
  ) +
  scale_size_manual(
    name = "",  # Legend title
    values = c("Pixels" = 0.5),  # Size for habitat mask points
    labels = c("Pixels in\nhabitat mask")  # Custom label for the points
  ) +
  theme_minimal()+
  labs(title = "Concavity = 5")+
  theme(
    legend.text = element_text(size = 14),  # Increase legend text size
    legend.title = element_text(size = 16),
    title = element_text(size = 20)
  )

twenty <- ggplot() + 
  geom_sf(data = MB22polycam20, aes(fill = "Camera Home Range"), color = NA) +  # Camera home range
  geom_sf(data = valuesmb22pointsfig, aes(size = "Pixels"), color = "black") +  # Habitat mask points
  scale_fill_manual(
    name = "",  # Legend title
    values = c("Camera Home Range" = "pink"),  # Fill colors
    labels = c("Camera home\nrange minimum\nconcave hull")  # Custom labels
  ) +
  scale_size_manual(
    name = "",  # Legend title
    values = c("Pixels" = 0.5),  # Size for habitat mask points
    labels = c("Pixels in\nhabitat mask")  # Custom label for the points
  ) +
  theme_minimal()+
  labs(title = "Concavity = 20")+
  theme(
    legend.text = element_text(size = 14),  # Increase legend text size
    legend.title = element_text(size = 16),
    title = element_text(size = 20)
  ) 

collar <- ggplot() + 
  geom_sf(data = MB22PeriphClip, aes(fill = "Collar Home Range"), color = NA,alpha =0.6)+
  geom_sf(data =valuesmb22pointsfig, size = 0.5)+ 
  scale_fill_manual(
    name = "",  # Legend title
    values = c("Collar Home Range" = "darkred"),  # Fill colors
    labels = c("Collar 95% KDE")  # Custom labels
  ) +
  labs(title="Collar Data-95% KDE")+
  theme_minimal()+
  theme(
    legend.text = element_text(size = 14),  # Increase legend text size
    legend.title = element_text(size = 16),
    title = element_text(size = 20)
  )


####Now print figures

suppfigures <- (collar | pointfive) / (one |two) / (five | twenty)
suppfigures



png("Figures/Supplemetary fig.png", width = 15, height = 20, res= 300, units = "in")

suppfigures
dev.off()
