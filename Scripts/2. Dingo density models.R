#2- Running null dingo density models 
library(tidyverse)
library(patchwork)
library(camtrapR)
library(ggplot2)
library(secr)
library(sf)
library(zoo)
#read in file with all detections


newtest <- read.csv(file = "Raw Data/combineddetectionsbothyears.csv",header = T)

newtest$DateTime <- as.POSIXct(newtest$DateTime, format = "%Y-%m-%d %H:%M")


#arrange
newtest <- newtest%>% arrange(DateTime)
newtest <- newtest %>%
  filter(Species == "Dingo")

#get only those 15mins apart
newtest15 <- newtest %>%
  group_by(Individual) %>%
  mutate(time_diff = c(0, diff(DateTime)),
         keep_row = row_number() == 1 | time_diff > 1800) %>%
  ungroup()

#keep only independent events in new df
newtest15 <- newtest15 %>% filter(keep_row)

#assign trap column from the file name

newtest15$Trap <- sub("^(.*?)__.*", "\\1", newtest15$File)

#first get correct dates:

dingodoubleids2023 <- newtest15 %>%
  filter(DateTime >= "2022-12-05 00:00" & DateTime <= "2023-02-20 00:00")

dingodoubleids2022 <- newtest15 %>%
  filter(DateTime >= "2021-12-05 00:00" & DateTime <= "2022-02-20 00:00")%>%
  mutate(Individual = if_else(Trap == "PS7" & Individual == "UOM1707", "UOM1701", Individual))

#then remove 'q' traps

dingodoubleids2023 <- dingodoubleids2023 %>%
  filter(!grepl("^Q", Trap))

dingodoubleids2022 <- dingodoubleids2022 %>%
  filter(!grepl("^Q", Trap))

#now get a summary of how many 'unidentified and night time'
dingodoubleids2022 %>%
  filter(!is.na(Individual) | Individual != "Unidentifiable") %>%
  summarise(n = n())

891-569

dingodoubleids2023 %>%
  filter(Individual == "Unidentifiable") %>%
  summarise(n = n())

dingodoubleids2022 <- dingodoubleids2022 %>%
  filter(!is.na(Individual) & Individual != "Unidentifiable")

dingodoubleids2023 <- dingodoubleids2023 %>%
  filter(!is.na(Individual) & Individual != "Unidentifiable" &!grepl("^Q", Trap))





#read in both files with camera information, and dingo independent events
camop22<- read.csv(file = "Raw Data/camop2022.csv", header = T)
camop23<- read.csv(file = "Raw Data/camop2023.csv", header = T)
trapsall <- read.csv(file = "Raw Data/TrapsUTMSpecific.csv", header = T)

trapsallPS2022 <- trapsall%>%
  filter(Session==1 & !Trap=='PS24' & !Trap=='PS23')

trapsallPS2023 <- trapsall%>%
  filter(Session==2 & !Trap=='PS26')


#make camera operation matrix

camopdingo2023 <- cameraOperation(CTtable = camop23,
                              stationCol = "Trap",
                              cameraCol = "Camera",
                              setupCol = "Setup_date",
                              retrievalCol = "Retrieval_date",
                              hasProblems = TRUE,
                              camerasIndependent = FALSE,
                              byCamera = FALSE,
                              allCamsOn = FALSE,
                              writecsv = FALSE,
                              occasionStartTime = 0,
                              dateFormat = "dmy")

camopdingo2022 <- cameraOperation(CTtable = camop22,
                                  stationCol = "Trap",
                                  cameraCol = "Camera",
                                  setupCol = "Setup_date",
                                  retrievalCol = "Retrieval_date",
                                  hasProblems = TRUE,
                                  camerasIndependent = FALSE,
                                  byCamera = FALSE,
                                  allCamsOn = FALSE,
                                  writecsv = FALSE,
                                  occasionStartTime = 0,
                                  dateFormat = "dmy")

#We need to first assign the covariates for sigma and g0 to the trapsall file. 



#Covariates done.  

#First need to sort files into  correct order: 
row_names2023 <- rownames(camopdingo2023)
row_names2022 <- rownames(camopdingo2022)

# Extract the prefix and numeric part from row names
prefix2023 <- gsub("[0-9]", "", row_names2023)
prefix2022 <- gsub("[0-9]", "", row_names2022)

numeric_part2023 <- as.numeric(gsub("\\D", "", row_names2023))
numeric_part2022 <- as.numeric(gsub("\\D", "", row_names2022))

# Create a sorting index based on prefix and numeric part
sort_index2023 <- order(prefix2023, numeric_part2023)
sort_index2022 <- order(prefix2022, numeric_part2022)

# Reorder the dataframe based on the sorting index
camopdingo2023 <- camopdingo2023[sort_index2023, , drop = FALSE]
camopdingo2022 <- camopdingo2022[sort_index2022, , drop = FALSE]

camopdingo2023[is.na(camopdingo2023)] <- 0



dingodoubleids2022 <- dingodoubleids2022%>%
  mutate(Pack = case_when(Individual == "UOF1501" ~ "JB",
                          Individual == "UOM2008" ~ "JB",
                          Individual == "UOM2001"~ "MB",
                          Individual == "UOM2002" ~ "MB",
                          Individual == "UOF1801" ~ "MB",
                          Individual == "UOM1701" ~ "MB",
                          Individual == "UOM2007" ~ "YG",
                          Individual == "SLF2003" ~ "SL",
                          Individual == "SLF2001" ~ "SL",
                          Individual == "UOM1707" ~ "WT",
                          Individual == "PCM1601" ~ "SL",
                          Individual == "SLF1501" ~ "SL",
                          Individual == "SLM2005" ~ "SL",
                          Individual == "MBF1701" ~ "YG",
                          Individual == "JBU2106" ~ "JB",
                          Individual == "UOF1906" ~ "WT",
                          Individual == "UOM2012" ~ "SB",
                          Individual == "UOF2013" ~ "SB",
                          Individual == "SLF2002" ~ "SL",
                          Individual == "JBU2101" ~ "JB",
                          Individual == "JBU2103" ~ "JB",
                          Individual == "JBU2104" ~ "JB",
                          Individual == "JBU2107" ~ "JB",
                          Individual == "WTU2103" ~ "WT",
                          Individual == "UOF1709" ~ "UNK",
                          Individual == "WTU2102" ~ "WT",
                          Individual == "UOF2009" ~ "UNK",
                          Individual == "WTU2101" ~ "WT",
                          Individual == "WTU2104" ~ "WT",
                          Individual == "UOF2011" ~ "SL",
                          Individual == "UOF2010" ~ "UNK",
                          Individual == "WTU2105" ~ "WT",
                          Individual == "SLM2104" ~ "SL",
                          Individual == "SLF1901" ~ "SL",
                          Individual == "SLF2102" ~ "SL",
                          Individual == "YGM2101" ~ "YG",
                          Individual == "YGM2102" ~ "YG"
                          ))


dingodoubleids2023 <- dingodoubleids2023%>%
  mutate(Pack = case_when(Individual == "MBF1701" ~ "YG",
                          Individual == "UOM2007" ~ "YG",
                          Individual == "YGM2102" ~ "YG",
                          Individual == "YGF2203" ~ "YG",
                          Individual == "UOF2101" ~ "UNK",
                          Individual == "WTU2205" ~ "WT",
                          Individual == "UOF1906" ~ "WT",
                          Individual == "UOM1707" ~ "WT",
                          Individual == "SLF1501" ~ "SL",
                          Individual == "SLF2201" ~ "SL",
                          Individual == "SLF2203" ~ "SL",
                          Individual == "SLF2204" ~ "SL",
                          Individual == "SLP2205" ~ "SL",
                          Individual == "UOM2002" ~ "SL",
                          Individual == "SLF2202" ~ "SL",
                          Individual == "UOM2004" ~ "YH",
                          Individual == "UOF1709" ~ "YH",
                          Individual == "WTU2105" ~ "WT",
                          Individual == "WTU2104" ~ "WT",
                          Individual == "WTU2202" ~ "WT",
                          Individual == "WTU2203" ~ "WT",
                          Individual == "UOF1801" ~ "MB",
                          Individual == "UOM2001" ~ "MB",
                          Individual == "WTU2201" ~ "WT",
                          Individual == "WTU2204" ~ "WT",
                          Individual == "YGM2201" ~ "YG",
                          Individual == "UOF2009" ~ "UNK",
                          Individual == "YGM2202" ~ "YG",
                          Individual == "UOM2012" ~ "SB",
                          Individual == "SBU2204" ~ "SB",
                          Individual == "SLF2102" ~ "SL",
                          Individual == "JBU2101" ~ "JB"
  ))







#Now can finally make spatial detection history 

dingodethist23 <- spatialDetectionHistory(recordTableIndividual = dingodoubleids2023,
                                         camOp = camopdingo2023,
                                         CTtable = trapsallPS2023,
                                         output = "binary",
                                         stationCol = "Trap",
                                         Xcol = "x", 
                                         Ycol = "y",
                                         species = "Dingo",
                                         stationCovariateCols = c("TrailType"),
                                         individualCol = "Individual",
                                         timeZone = "Australia/Sydney",
                                         recordDateTimeCol = "DateTime", 
                                         recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                                         occasionLength = 1,
                                         maxNumberDays = 77,
                                         day1 = "survey",
                                         includeEffort = FALSE)


dingodethist22 <- spatialDetectionHistory(recordTableIndividual = dingodoubleids2022,
                                          camOp = camopdingo2022,
                                          CTtable = trapsallPS2022,
                                          output = "binary",
                                          stationCol = "Trap",
                                          Xcol = "x", 
                                          Ycol = "y",
                                          species = "Dingo",
                                          stationCovariateCols = c("TrailType"),
                                          individualCol = "Individual",
                                          timeZone = "Australia/Sydney",
                                          recordDateTimeCol = "DateTime", 
                                          recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                                          occasionLength = 1,
                                          day1 = "survey",
                                          maxNumberDays = 77,
                                          includeEffort = FALSE)

#Done. Now should have all elements for a secr model. 



#lets run SECR model. USING HN det function for both. 

HN2022 <- secr.fit(dingodethist22, 
                   mask = maskclippedforpaper2023,
                   link = "log",
                   detectfn = "HN",
                   ncores = 4,
                   trace = TRUE,
                   model = list(D ~1, sigma ~1, g0 ~ h2),
                   details = list(fastproximity = FALSE))

HN2023 <- secr.fit(dingodethist23, 
                   mask = maskclippedforpaper2023,
                   link = "log",
                   detectfn = "HN",
                   ncores = 4,
                   trace = TRUE,
                   model = list(D ~1, sigma ~1, g0 ~ h2),
                   details = list(fastproximity = FALSE))






options(scipen=999)

summary(HN2022)
summary(HN2023)

#ran models with h2 for unmodelled heterogeneity in detection rates between two potential classes of individuals (latent)

#Then below we check with monte carlo simulations of the goodness of fit. 

GOF2022 <- MCgof(HN2022,nsim = 1000)
GOF2023 <- MCgof(HN2023,nsim = 1000)
GOF2022
GOF2023
plot(GOF2022)
plot(GOF2023)

plot(GOF2022)
plot(GOF2023)

#export for supplementary materials

###Models are run. Now need to go into next scripts. write p results!!!