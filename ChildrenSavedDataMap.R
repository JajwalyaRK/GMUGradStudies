install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata", "rworldmap")



library(tidyverse)
library(ggplot2)
theme_set(theme_bw())
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(cartography)
library(rgdal)
library(rworldmap)
library(dplyr)

#world <- ne_countries(scale = "medium", returnclass = "sf")
#class(world)

#ggplot(data = world) +
#  geom_sf() +
#  xlab("Longitude") + ylab("Latitude") +
#  ggtitle("World map", subtitle = paste0("(", length(unique(world$NAME)), " countries)"))

childrenSaved <- read.csv("WorldMillions of Children's Lives Have Been Saved.csv", check.names=FALSE, stringsAsFactors=FALSE)
str(childrenSaved)
head(childrenSaved)
view(childrenSaved)
#filter(year %in% c(1970, 1990, 2010, 2030))

x<-childrenSaved$World
y<-childrenSaved$Year

plot(x, y)

#barGraph <- ggplot(childrenSaved, aes(x = "Year") + geom_line(stat="identity")

### Requires country code in data
# mapped_data <- joinCountryData2Map(childrenSaved, joinCode = "ISO3", nameJoinColumn = "change.1990.2017")
# par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
# mapCountryData(mapped_data, nameColumnToPlot = "Year")

### Requires Shapefile data 
# choroLayer(spdf = childrenSaved, 
#           df = childrenSaved, 
#           var = "Year", 
#           #breaks = c(0, 10, 30, 50, 70, 90, 110, 150), 
#           col = cols, 
#           border = "grey40", 
#           lwd = 0.5, 
#           legend.pos = "left",
#           legend.title.txt = "Number of Children Saved", 
#           legend.values.rnd = 10,
#           add = TRUE)
