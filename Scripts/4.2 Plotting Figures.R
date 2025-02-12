#4.1 plotting te results
library(ggmap)

###we're going to merge columns to make plotting with colours easier. 

#start with 22, and collar territory zone and pack 

#assign factor for ordering
valuescentres2022$D.sum.category <- factor(valuescentres2022$D.sum.category, levels = c("0-0.002", "0.002-0.004","0.004-0.006","0.006-0.008","0.008-0.010","0.010-0.012","0.012-0.014",">0.014"))

valuescentres2023$D.sum.category <- factor(valuescentres2023$D.sum.category, levels = c("0-0.002", "0.002-0.004","0.004-0.006","0.006-0.008","0.008-0.010","0.010-0.012","0.012-0.014",">0.014"))

valuescentres2022$CollarPack <- factor(valuescentres2022$CollarPack, levels = c("None", "JB", "WT","SL","MB","YG"))
valuescentres2023$CollarPack <- factor(valuescentres2023$CollarPack, levels = c("None", "JB", "WT","SL","MB","YG"))

valuescentres2022$CamPack <- factor(valuescentres2022$CamPack, levels = c("None","JB", "WT","SL","MB","YG","SB"))
valuescentres2023$CamPack <- factor(valuescentres2023$CamPack, levels = c("None","JB", "WT","SL","MB","YG"))



###For plots 1 and 5, we need to seperate the dataset down into just core regions, from collarpack. 
corevaluescentres2022 <- valuescentres2022 %>%
  mutate(CollarPack = case_when(!TerritoryZoneCollar == "Core"~"None",
                                TRUE~CollarPack))
  
corevaluescentres2023 <- valuescentres2023%>%
  mutate(CollarPack = case_when(!TerritoryZoneCollar == "Core"~"None",
                                TRUE~CollarPack))

corevaluescentres2022$CollarPack <- factor(corevaluescentres2022$CollarPack, levels = c("None", "JB", "WT","SL","MB","YG","SB"))

corevaluescentres2023$CollarPack <- factor(corevaluescentres2023$CollarPack, levels = c("None", "JB", "WT","SL","MB","YG"))



pack_colors_cams <- c("None" = "lightgrey", 
                        "JB" = "#FF3300",  
                        "WT" = "#00BFC4", 
                        "SL" = "#FFCC33",
                        "MB" = "#FF9CFF",
                        "YG" = "#66FF33",
                        "SB" = "#3369CC")


#just to make JB bigger for this figure:
corevaluescentres2022$size_var <- ifelse(corevaluescentres2022$CollarPack == "JB", 6, 2)


####Core ranges from collars, fig 1

plot22collarcore <- ggplot(corevaluescentres2022, aes(x = x, y = y, color = CollarPack, size = size_var))+
                             geom_point() +
  scale_color_manual(values = pack_colors_cams)+
  scale_size_identity() +
  geom_point(data = trapsallPS2022, aes(x = x, y = y), shape = 17, size = 3, color = "black") +
  labs(title = "                    GPS Collars",
       x = "x",
       y = "y",
       color = "Core ranges\n-50% KDE") +
  theme_minimal()+
  theme(plot.margin = margin(1,1,1,1, "cm"),
        panel.background = element_blank(),
        axis.title = element_text(size = 20),   # Adjust axis title size
        axis.text = element_text(size = 13),    # Adjust axis text size
        axis.text.x = element_text(size = 17),  # Adjust x-axis text size
        axis.text.y = element_text(size = 17),
        legend.text = element_text(size = 17),
        legend.title = element_text(size = 20),
        title = element_text(size = 20))+
  annotate("text", x=-Inf,y=Inf, label = "a)",hjust = -0.1, vjust=1.5, size = 6)


plot22collarcore


####`````````` Activity centres fig 2,2022

plot22cam <- ggplot(valuescentres2022, aes(x = x, y = y, color = D.sum.category)) +
  scale_color_manual("Predicted Activity\nCentre Density-\nPossible Cores",values = c("Grey","Yellow","#FFB84D","Orange","DarkOrange", "Red","DarkRed","Black"))+
  geom_point(size = 2) +
  geom_point(data = trapsallPS2022, aes(x = x, y = y), shape = 17, size = 3, color = "black") +
  labs(x = "x",
       y = "y",
       title = "                      Cameras") +
  theme_minimal()+
  theme(plot.margin = margin(1,1,1,1, "cm"),
        panel.background = element_blank(),
        axis.title = element_text(size = 20),   # Adjust axis title size
        axis.text = element_text(size = 13),    # Adjust axis text size
        axis.text.x = element_text(size = 17),  # Adjust x-axis text size
        axis.text.y = element_text(size = 17),
        legend.text = element_text(size = 17),
        legend.title = element_text(size = 20),
        title = element_text(size =20))+
  annotate("text", x=-Inf,y=Inf, label = "b)",hjust = -0.1, vjust=1.5, size = 6)

plot22cam

####~~~~~~~~~~~Collar home ranges Fig 3,2022

plot22collar <- ggplot(valuescentres2022, aes(x = x, y = y, color = CollarPack)) +
  geom_point(size = 2) +
  geom_point(data = trapsallPS2022, aes(x = x, y = y), shape = 17, size = 3, color = "black") +
  scale_color_manual(values = pack_colors_cams)+
  labs(
       title = "                     GPS Collars",
       x = "x",
       y = "y",
       color = "Peripheral ranges\n-95% KDE") +
  theme_minimal()+
  theme(plot.margin = margin(1,1,1,1, "cm"),
        panel.background = element_blank(),
        axis.title = element_text(size = 20),   # Adjust axis title size
        axis.text = element_text(size = 13),    # Adjust axis text size
        axis.text.x = element_text(size = 17),  # Adjust x-axis text size
        axis.text.y = element_text(size = 17),
        legend.text = element_text(size = 17),
        legend.title = element_text(size = 20),
        title = element_text(size = 20))+
  annotate("text", x=-Inf,y=Inf, label = "a)",hjust = -0.1, vjust=1.5, size = 6)


plot22collar

####Fig 4, home ranges from camera data, 2022


plot22camHR <- ggplot(valuescentres2022, aes(x = x, y = y, color = CamPack)) +
  scale_color_manual(values = pack_colors_cams,
                     labels = c("None","JB","WT","SL","MB","YG","Unknown"))+
  geom_point(size = 2) +
  geom_point(data = trapsallPS2022, aes(x = x, y = y), shape = 17, size = 3, color = "black") +
  
  labs(x = "x",
       y = "y",
       color = "Peripheral ranges\n- KDEs",
       title = "                  Cameras") +
  theme_minimal()+
  theme(plot.margin = margin(1,1,1,1, "cm"),
        panel.background = element_blank(),
        axis.title = element_text(size = 20),   # Adjust axis title size
        axis.text = element_text(size = 13),    # Adjust axis text size
        axis.text.x = element_text(size = 17),  # Adjust x-axis text size
        axis.text.y = element_text(size = 17),
        legend.text = element_text(size = 17),
        legend.title = element_text(size = 20),
        title = element_text(size = 20))+
  annotate("text", x=-Inf,y=Inf, label = "b)",hjust = -0.1, vjust=1.5, size = 6)

plot22camHR


####~~~~~~~~Fig 5 -  core home ranges

plot23collarcore <- ggplot(corevaluescentres2023, aes(x = x, y = y, color = CollarPack)) +
  geom_point(size = 2) +
  geom_point(data = trapsallPS2022, aes(x = x, y = y), shape = 17, size = 3, color = "black") +
  scale_color_manual(values = pack_colors_cams)+
  labs(x = "x",
       y = "y",
       color = "Core ranges-\n50% KDE",
       title = "                  GPS Collars") +
  theme_minimal()+
  theme(plot.margin = margin(1,1,1,1, "cm"),
        panel.background = element_blank(),
        axis.title = element_text(size = 20),   # Adjust axis title size
        axis.text = element_text(size = 13),    # Adjust axis text size
        axis.text.x = element_text(size = 17),  # Adjust x-axis text size
        axis.text.y = element_text(size = 17),
        legend.text = element_text(size = 17),
        legend.title = element_text(size = 20),
        title = element_text(size = 20))+
  annotate("text", x=-Inf,y=Inf, label = "a)",hjust = -0.1, vjust=1.5, size = 6)


plot23collarcore




####`````````` Activity centres fig 6, 2023

plot23cam <- ggplot(valuescentres2023, aes(x = x, y = y, color = D.sum.category)) +
  scale_color_manual("Predicted Activity\nCentre Density-\nPossible Cores",values = c("Grey","Yellow","#FFB84D","Orange","DarkOrange", "Red","DarkRed","Black"))+
  geom_point(size = 2) +
  geom_point(data = trapsallPS2022, aes(x = x, y = y), shape = 17, size = 3, color = "black") +
  labs(x = "x",
       y = "y",
       title = "                     Cameras") +
  theme_minimal()+
  theme(plot.margin = margin(1,1,1,1, "cm"),
        panel.background = element_blank(),
        axis.title = element_text(size = 20),   # Adjust axis title size
        axis.text = element_text(size = 13),    # Adjust axis text size
        axis.text.x = element_text(size = 17),  # Adjust x-axis text size
        axis.text.y = element_text(size = 17),
        legend.text = element_text(size = 17),
        legend.title = element_text(size = 20),
        title = element_text(size = 20))+
  annotate("text", x=-Inf,y=Inf, label = "b)",hjust = -0.1, vjust=1.5, size = 6)

plot23cam


####~~~~~~~~~~~Collar home ranges Fig 7, 2023

plot23collar <- ggplot(valuescentres2023, aes(x = x, y = y, color = CollarPack)) +
  geom_point(size = 2) +
  geom_point(data = trapsallPS2022, aes(x = x, y = y), shape = 17, size = 3, color = "black") +
  scale_color_manual(values = pack_colors_cams)+
  labs(x = "x",
       y = "y",
       color = "Peripheral ranges\n-95% KDE",
       title = "                  GPS Collars") +
  theme_minimal()+
  theme(plot.margin = margin(1,1,1,1, "cm"),
        panel.background = element_blank(),
        axis.title = element_text(size = 20),   # Adjust axis title size
        axis.text = element_text(size = 13),    # Adjust axis text size
        axis.text.x = element_text(size = 17),  # Adjust x-axis text size
        axis.text.y = element_text(size = 17),
        legend.text = element_text(size = 17),
        legend.title = element_text(size = 20),
        title = element_text(size = 20))+
  annotate("text", x=-Inf,y=Inf, label = "a)",hjust = -0.1, vjust=1.5, size = 6)


plot23collar

#### home ranges from camera data, fig 8, 2023


plot23camHR <- ggplot(valuescentres2023, aes(x = x, y = y, color = CamPack)) +
  scale_color_manual(values = pack_colors_cams,
                     labels = c("None","JB","WT","SL","MB","YG"))+
  geom_point(size = 2) +
  geom_point(data = trapsallPS2022, aes(x = x, y = y), shape = 17, size = 3, color = "black") +
  labs(x = "x",
       y = "y",
       color = "Peripheral ranges\n- KDEs",
       title = "                  Cameras") +
  theme_minimal()+
  theme(plot.margin = margin(1,1,1,1, "cm"),
        panel.background = element_blank(),
        axis.title = element_text(size = 20),   # Adjust axis title size
        axis.text = element_text(size = 13),    # Adjust axis text size
        axis.text.x = element_text(size = 17),  # Adjust x-axis text size
        axis.text.y = element_text(size = 17),
        legend.text = element_text(size = 17),
        legend.title = element_text(size = 20),
        title = element_text(size = 20))+
  annotate("text", x=-Inf,y=Inf, label = "b)",hjust = -0.1, vjust=1.5, size = 6) 
        
plot23camHR


twentytwo <- (plot22collarcore | plot22cam) / (plot22collar |plot22camHR)
twentytwo

twentythree <- (plot23collarcore | plot23cam) / (plot23collar |plot23camHR)
twentythree


twentytwoa <- (plot22collarcore / plot22cam) 
twentytwoa
twentytwob <-  (plot22collar / plot22camHR)
twentytwob

twentythreea <- (plot23collarcore / plot23cam)
twentythreea
twentythreeb <- (plot23collar / plot23camHR)
twentythreeb


##print plots above one another. 

png("Figures/2022a.png", width = 10, height = 15, res= 300, units = "in")
twentytwoa
dev.off()

png("Figures/2022b.png", width = 10, height = 15, res= 300, units = "in")
twentytwob
dev.off()

png("Figures/2023a.png", width = 10, height = 15, res= 300, units = "in")
twentythreea
dev.off()

png("Figures/2023b.png", width = 10, height = 15, res= 300, units = "in")
twentythreeb
dev.off()


