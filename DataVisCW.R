install.packages(c("sf", "rnaturalearth", "rnaturalearthdata", "rgeos",
                   "eurostat", "tidyverse", "cartogram", "s2"))

install.packages("ggthemes")
install.packages("readxl") 
library(readxl)
library(ggplot2)
library(sf)
library(dplyr) 
library(rnaturalearth)
library(scales)
library(ggthemes)


utla.map <- st_read(
  "/Counties_and_Unitary_Authorities_December_2019_FEB_UK_2022_-960929472398567633-2/Counties_and_Unitary_Authorities_December_2019_FEB_UK.shp")

ldn.map <- utla.map[substr(utla.map$ctyua19cd, 1, 3) == "E09",]


ggplot(ldn.map) + geom_sf() +
  theme_bw() +
  coord_sf(crs = st_crs(4326))


annual_variance <- read_excel("/Borough location.xlsx","Sheet6")
annual_variance$annual_var <- percent(annual_variance$annual_var, accuracy = 0.01)


ldn.map <- right_join(annual_variance, ldn.map, by = c("id" = "ctyua19cd"))

#ANNUAL VARIANCE IN PRIVATE RENTS BY BOROUGH

#=================================================================================
  ggplot(ldn.map) + geom_sf(aes(geometry = geometry)) + aes(fill = factor(group)) +
    theme_bw() +
    scale_fill_brewer(labels = c("12% and above"),palette = "PiYG") +
    geom_sf_text(aes(label = Borough,geometry = geometry),fun.geometry = st_centroid) +
    theme(panel.border = element_blank(), panel.grid.major = element_blank()) +
    theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none",legend.title = element_text(size = 15), legend.key.size = unit(1,'cm'), legend.text = element_text(size = 15), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    #ggtitle("Boroughs with 12% or above increase in private rents from May 2022 to May 2023") + theme(plot.title = element_text(hjust=0.5,size = 14)) +
    labs(fill = "Annual price variance")  
#=================================================================================



#SALES VOLUMES BY BOROUGH


ldn.map2 <- utla.map[substr(utla.map$ctyua19cd, 1, 3) == "E09",]

sales_volume <- read_excel("/Borough location.xlsx","Sheet5")
sales_volume$Variation <- percent(sales_volume$Variation, accuracy = 0.01)

ldn.map2 <- right_join(sales_volume, ldn.map2, by = c("id" = "ctyua19cd"))

#=================================================================================
ggplot(ldn.map2) + geom_sf(aes(geometry = geometry)) + aes(fill = factor(group)) +
  theme_bw() +
  scale_fill_brewer(labels = c("100%", "200%", "300%", "400%")) +
  #geom_sf_text(aes(label = Borough,geometry = geometry),fun.geometry = st_centroid) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank()) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.title = element_text(size = 15), legend.key.size = unit(1,'cm'), legend.text = element_text(size = 15), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), 
        axis.ticks.y=element_blank()) +
  #ggtitle("Boroughs with highest increase in sales of houses July 2021 to July 2022") + theme(plot.title = element_text(hjust=0.5,size = 14)) +
  labs(fill = "Sales volume variance") 

#DUMBELL PLOT
#=================================================================================
install.packages("ggalt")
library(ggalt)

sales_volume <- read_excel("/Sales.xlsx")

ggplot(sales_volume, aes(y = Borough, x = sales, xend = end_2022)) +
  geom_dumbbell(color = "darkgray",  # Color of the line
                size = 1,            # Line width
                dot_guide = FALSE,   # Whether to add a guide from origin to X or not
                size_x = 3,          # Size of the X point
                size_xend = 3,       # Size of the X end point
                colour_x = "#F69541",    # Color of the X point
                colour_xend = "#699DC6") +
  geom_text(color="black", size=2, hjust=2,
            aes(x=sales, label=sales))+
  geom_text(aes(x=end_2022, label=end_2022), 
            color="black", size=2, hjust=-0.5)+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank()) 
    #ggtitle("Boroughs with highest increase in sales of houses July 2021 to July 2022") + theme(plot.title = element_text(hjust=0.5,size = 14))
 
#=================================================================================






