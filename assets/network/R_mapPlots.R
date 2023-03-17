setwd("/home/tmoore/Dropbox/ProfPort/Personal_Webpage")

# Load packages -----
#library(ggmap) # For us
library(maptools)
library(maps)
library(readODS)
library(viridis)
library(RColorBrewer)
library(ggrepel)
library(ggswissmaps)
library(dplyr)


# Create points data -----
df <- data.frame("host" = c("Clemson University",
                            "Adolphe Merkle Institute",
                            "Istituto Italiano di Tecnologia"),
                 "host_col" = c("#F66733", "#FAA630", "#0077B3"),
                 "lat" = c(34.676329, 46.792908, 44.475168),
                 "lon" = c(-82.837320, 7.154900, 8.906257))
df$host <- factor(df$host, levels = c("Clemson University",
                                      "Adolphe Merkle Institute",
                                      "Istituto Italiano di Tecnologia"))

# Generate map -----
set.seed(1987)
mp <- NULL
mapWorld <- borders("world", colour="grey40", fill="whitesmoke", 
                    lwd = 0.20)
mp <- ggplot(df, aes(lon, lat)) + mapWorld +
#  geom_path(lwd = 0.25, colour = "#009DF8",# MINDED Light Blue
#            alpha = I(0.66))+
  geom_point(size = 1.5, stroke = 0.25, shape = 21,
             fill = df$host_col)+
  geom_text_repel(data = df, aes(label = host), size = 2,
                  color = "black")+
  scale_fill_manual(values = c("#F66733",#Clemson 
                              "#FAA630",#AMI 
                              "#0077B3"#, IIT
                              ))+
  scale_x_continuous(limits = c(-195,195))+#, breaks = seq(-200, 200, 20))+
  scale_y_continuous(limits = c(-82, 82))+#, breaks = seq(-80, 80, 20))+
  theme(aspect.ratio = 0.55, axis.title = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "aliceblue"),
        panel.grid = element_blank())
mp
ggsave("worldMap.pdf", device = "pdf", dpi = 600,
       width = 35, height = 20, units = c("cm"),
       plot = last_plot())

# Sub-plots -----
## South Carolina -----
scMap_state <- map_data("state") %>% subset(region == "south carolina")
scMap_county <- map_data("county") %>% subset(region == "south carolina")

p_scMap <- ggplot()+
  coord_fixed(1.3)+
  geom_polygon(data = scMap_state, mapping = aes(long, lat, group=group),
               color = "black", fill = "gray")+
  geom_polygon(data = scMap_county, mapping = aes(long, lat, group=group),
               fill = NA, colour = "white")+
  geom_polygon(data = scMap_state, mapping = aes(long, lat, group=group),
               color = "black", fill = NA)+
  geom_point(data = df[which(df$host == "Clemson University"),],
             mapping = aes(lon, lat),
             shape = 21, colour = "black", fill = "#F66733",
             size = 4)+
  scale_x_continuous(limits = c(-83.37,-78.55), expand = c(0,0))+
  scale_y_continuous(limits = c(32.0, 35.21), expand = c(0,0))+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = NA),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank())
p_scMap
ggsave("plot_scClemson.png", device = "png", units = c("cm"),
       width=10, height=8.3, dpi=300)

## Switzerland -----
data("shp_df")
class(shp_df)
chMap <- shp_df$g1l15
chMap_kanton <- shp_df$g1k15
rm(shp_df)

p_chMap <- ggplot()+
  coord_fixed(1)+
  geom_polygon(data = chMap, mapping = aes(long, lat, group=group),
               colour = "black", fill = "gray")+
  geom_polygon(data = chMap_kanton, mapping = aes(long, lat, group=group),
               colour = "white", fill = NA)+
	geom_polygon(data = chMap, mapping = aes(long, lat, group=group),
               colour = "black", fill = NA)+
	geom_point(mapping = aes(579000, 183000), shape=21, 
						 colour = "black", fill = "#FAA630", size=3)+
	scale_x_continuous(limits = c(480000,833840), expand = c(0,0))+
	scale_y_continuous(limits = c(75280,295940), expand = c(0,0))+
	theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = NA),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank())
p_chMap
ggsave("plot_chAMI.png", device = "png", units = c("cm"),
       width=10, height=6.33, dpi=300)

## Italy -----
itMap <- map_data("italy")
itMap_country <- map_data("world")
itMap_country <- itMap_country[which(itMap_country$region == "Italy"),]

p_itMap <- ggplot()+
  coord_fixed(1.3)+
  geom_polygon(data = itMap_country, mapping = aes(long, lat, group=group),
               colour = "black", fill = "gray")+
  geom_polygon(data = itMap, mapping = aes(long, lat, group=group),
               color = "white", fill = NA)+
  geom_polygon(data = itMap_country, mapping = aes(long, lat, group=group),
               colour = "black", fill = NA)+
  geom_point(data = df[which(df$host == "Istituto Italiano di Tecnologia"),],
             mapping = aes(lon, lat), shape = 21, fill = "#0077B3",
             size = 4)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(limits = c(36.6, 47.1), expand = c(0,0))+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = NA),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank())
p_itMap
ggsave("plot_itIIT.png", device = "png", units = c("cm"),
       width=8.3, height=9, dpi=300)

