library(tidyverse)

lives <- read.csv("Children Deaths.csv", header = TRUE, stringsAsFactors=FALSE)
view(lives)
str(lives)

colnames(lives)[2] <- "E. Asia & Pacific"
colnames(lives)[3] <- "Eurasia"
colnames(lives)[4] <- "Latin America & Carribean"
colnames(lives)[5] <- "South Asia"
colnames(lives)[6] <- "Middle East & N. Africa"
colnames(lives)[7] <- "N. America"
colnames(lives)[8] <- "Sub Saharan Africa"

meltedLives <- reshape2::melt(lives, id.var='Year')
view(meltedLives)

library(ggplot2)
library(wesanderson)
library(RColorBrewer)
library(ggthemes)
library(ggrepel)
library(directlabels)
library(scales)

lifePlot <- ggplot(meltedLives, 
                   aes(x = Year, y = value, col = variable)) + 
  geom_line(size = 2, alpha = 2) +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 3000), expand = c(0, 0)) +
  scale_y_continuous(labels = label_comma())+
  coord_cartesian(ylim=c(0, 10000000)) +
  labs(title="Millions of Children's Lives Have Been Saved", 
       subtitle="Due to better healthcare, education, & social conditions", x=element_blank(),
       y="Mortality Rate", 
       col = "Countries",
       caption="Sources: GapMinder, OurWorldinData.org") +
  scale_color_calc() +
  theme(legend.position="none") +
  geom_dl(aes(colour = variable, label = variable), method = list(dl.trans(x = x + 0), "smart.grid")) 

lifePlot

########################################################################################################################
# gt1 <- ggplotGrob(lifePlot)
# gt1$layout$clip[gt1$layout$name == "panel"] <- "off"
# grid.draw(gt1)


#direct.label(lifePlot, "bottom.pieces")
#angled.boxes <- list(#"far.from.others.borders","calc.boxes","enlarge.box","top.bumpup",
#vjust=-.75, hjust=-1, cex=1, alpha=0.75, fontface="bold")
#direct.label(lifePlot, angled.boxes) 


  #theme_fivethirtyeight() +
  #geom_dl(aes(label = variable), method = list(dl.combine("last.points")))
  #geom_label_repel(aes(label = variable),
  #                 nudge_x = 1,
  #                 na.rm = TRUE)



##############################################################################
drops <- c("source","URL")
l<-lives[ , !(names(lives) %in% drops)] 
view(l)
transposed <- t(l)
view(transposed)
str(transposed)
head(transposed)


EAsiaPacific <- transposed[, -c(3:10)]
Eurasia <- transposed[, -c(2, 4:10)]
LACarr <- transposed[, -c(2:3, 5:10)]
SouthAsia <- transposed[, -c(2:4, 6:10)]
MENA <- transposed[, -c(2:5, 7:10)]
NAmer <- transposed[, -c(2:6, 8:10)]
Afr <- transposed[, -c(2:7, 9:10)]
W <- transposed[, -c(2:9)]
view(W)
str(SouthAsia)

plot(transposed)
library(reshape2)

cSaved <- melt(transposed, id = c("Year"))
view(cSaved)
transposed$row <- seq_len(nrow(transposed))
data2 <- melt(transposed, id.vars = "row")

view(data2)











