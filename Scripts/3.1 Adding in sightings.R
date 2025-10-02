#3.1 - Comparing this dataset from cameras to dataset from sightings. 
library(tidyverse)

sightings <- read.csv(file = "Raw Data/SightingsDataset.csv",header = T)

sightings$Date <- as.Date(strptime(sightings$Date, "%d-%m-%Y"))

sightings2122 <- sightings%>%
  filter(Date >= "2021-09-05" & Date <= "2022-06-04 00:00")%>%
  separate_rows(IDs, sep = " ") %>%
  rename(Individual = IDs)%>%
  filter(Record_type=="Researcher",
         !Pack=="None",
         !Individual=="" ,
         !Pack=="",
         !Pack=="NA",
         !Individual=="Unknown",
         !Pack=="MB/SL",
         !Pack=="UNK",
         !Individual %in% c('SLM210', 'SLF2102'))

Individuals2122 <- sightings2122%>%
  group_by(Pack)%>%
  summarise(CountSightings = n_distinct(Individual),
            IndividualSightings = paste(unique(Individual), collapse = ", "))

Individualsbothmethods2122 <- Individuals2122 %>%
  left_join(DetsEachCam22, by = "Pack")%>%
  mutate(Year="2022")


#filter data as totally stuffed records.
sightings2223 <- sightings%>%
  filter(Date >= "2022-09-05" & Date <= "2023-06-04 00:00")%>%
  separate_rows(IDs, sep = " ") %>%
  rename(Individual = IDs)%>%
  filter(Record_type=="Researcher",
         !Individual=="" ,
         !Pack=="",
         !Pack=="UNK",
         !Individual=="UOM1801",
         !Individual=="Unknown",
         !Individual %in% c('SLP22?', 'SLP?', 'SLF2203', 'SLF2204', 'SLM2201','SLF20?'))


Individuals2223 <- sightings2223%>%
  group_by(Pack)%>%
  summarise(CountSightings = n_distinct(Individual),
            IndividualSightings = paste(unique(Individual), collapse = ", "))


Individualsbothmethods2223 <- Individuals2223 %>%
  left_join(DetsEachCam23, by = "Pack")%>%
  mutate(Year="2023")

Individualsbothmethodsbothyears <- rbind(Individualsbothmethods2122,Individualsbothmethods2223)
Individualsbothmethodsbothyears

#Make nicer 
Individualsbothmethodsbothyears <- Individualsbothmethodsbothyears %>%
  mutate(IndividualSightings = str_replace_all(IndividualSightings, 
                                               c("JBM2101" = "JBU2101", 
                                                 "JBF2103" = "JBU2103", 
                                                 "JBF2106" = "JBU2106",
                                                 "SLU2102" = "SLF2102", 
                                                 "SLF2201" = "SLP2201", 
                                                 "SLF2202" = "SLP2202",
                                                 "SLF2203" = "SLP2203", 
                                                 "SLF2204" = "SLP2204", 
                                                 "WTF2102" = "WTU2102")),
         IndividualPopSurvey = str_replace_all(IndividualPopSurvey, 
                                               c("JBM2101" = "JBU2101", 
                                                 "JBF2103" = "JBU2103", 
                                                 "JBF2106" = "JBU2106",
                                                 "SLU2102" = "SLF2102", 
                                                 "SLF2201" = "SLP2201", 
                                                 "SLF2202" = "SLP2202",
                                                 "SLF2203" = "SLP2203", 
                                                 "SLF2204" = "SLP2204", 
                                                 "WTF2102" = "WTU2102"))
  )

Individualsbothmethodsbothyears <- Individualsbothmethodsbothyears %>%
  rowwise() %>%
  mutate(
    Both = str_c(unique(c(
      str_split(IndividualSightings, ",")[[1]],
      str_split(IndividualPopSurvey, ",")[[1]]
    )), collapse = ",")
  ) %>%
  ungroup()

Individualsbothmethodsbothyears$Both <- sapply(Individualsbothmethodsbothyears$Both, function(x) {
  unique_values <- unique(trimws(unlist(strsplit(x, ",\\s*"))))
  paste(unique_values, collapse = ", ")
})

Individualsbothmethodsbothyears <- Individualsbothmethodsbothyears %>%
  rowwise() %>%
  mutate(UniqueCount = length(unique(unlist(str_split(Both, ","))))) %>%
  ungroup()%>%
  mutate(proportioncams = (CountPopSurvey / UniqueCount)*100)

write.csv(Individualsbothmethodsbothyears, file = "Derived Data/totalsboth.csv")                       

