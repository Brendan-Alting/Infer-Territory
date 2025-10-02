#Plot study area 
library(ggplot2)
library(cowplot)
library(ggmap)
library(rnaturalearth)
register_stadiamaps("f9d511a5-33ca-42aa-a527-943c07b6fe0f", write = FALSE)



trapsplot <- st_as_sf(trapsallPS2022, coords = c("x","y"), crs = 32756)

trap_sf_wgs_84 <- st_transform(trapsplot, crs = 4326)
trapsplotlat <- st_coordinates(trap_sf_wgs_84)
trapsplotlat <- as.data.frame(trapsplotlat)
names(trapsplotlat) <- c("Longitude", "Latitude")
trapsplotlat_final <- cbind(st_drop_geometry(trap_sf_wgs_84), trapsplotlat)

#Aus inset

###### Plot all data together for illustration of territories

###Get aus inset map
australia_map <- ne_states(country = "Australia", returnclass = "sf")

####Create a little point where we are. 
#get bounding box of locations: 
bbox <- st_bbox(trap_sf_wgs_84)

#expand by tiny bit
expanded_bbox <- bbox + c(-0.01, -0.01, 0.01, 0.01)

bbox_df <- data.frame(
  xmin = expanded_bbox[["xmin"]],
  xmax = expanded_bbox[["xmax"]],
  ymin = expanded_bbox[["ymin"]],
  ymax = expanded_bbox[["ymax"]]
)

##then make true polygon
bbox_polygon <- st_as_sf(
  st_sfc(
    st_polygon(list(matrix(c(
      bbox_df$xmin, bbox_df$ymin,
      bbox_df$xmin, bbox_df$ymax,
      bbox_df$xmax, bbox_df$ymax,
      bbox_df$xmax, bbox_df$ymin,
      bbox_df$xmin, bbox_df$ymin
    ), ncol = 2, byrow = TRUE)))
  )
)

st_crs(bbox_polygon)  # Check CRS of your cleaned points

st_crs(bbox_polygon) <- st_crs(australia_map)

australia_inset <- ggplot() +
  geom_sf(data = australia_map, fill = "lightgrey", color = "black") +
  geom_sf(data = bbox_polygon, color = "red", lwd=1) + 
  coord_sf(xlim = c(140, 154), ylim = c(-39, -28)) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.6),         panel.spacing = margin(10, 10, 10, 10))

australia_inset



#get base map
map <- get_stadiamap(bbox = c(left = 152.12, 
                              bottom = -32.73,
                              right = 152.56, 
                              top = -32.38),
                     crop = TRUE,
                     zoom = 10, maptype = "stamen_terrain_background")

basemapgg <- ggmap(map)
plot(basemapgg)

studyareplot <- basemapgg +
  geom_sf(data = trap_sf_wgs_84, fill = "black", inherit.aes = FALSE, size = 6, shape = 21) +
  geom_sf_text(data = trap_sf_wgs_84, aes(label = Trap),inherit.aes = FALSE, size = 5, color = "black",nudge_x = -0.014,nudge_y = 0.005)+
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude")+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size =19),
        panel.grid = element_blank(),
        panel.border = element_rect(color = "red", fill = NA, size = 1),
        legend.position = "none") 

habmapplot <- ggdraw()+
  draw_plot(studyareplot)+
  draw_plot(australia_inset,0.6, 0.15, 0.35, 0.35)

habmapplot


png("Figures/habmap.jpg", width = 11, height =15, res= 300, units = "in")


habmapplot


dev.off()

