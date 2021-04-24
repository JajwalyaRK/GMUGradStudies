library(tidyverse)

lives <- read.csv("Millions of Children's Lives Have Been Saved - Regional breakdown.csv", header = TRUE, stringsAsFactors=FALSE)
view(lives)

drops <- c("source","URL")
l<-lives[ , !(names(lives) %in% drops)] 

transposed <- t(l)
view(transposed)
str(transposed)
head(transposed)


library(reshape2)

cSaved <- melt(transposed, id = c("Year"))
view(cSaved)
transposed$row <- seq_len(nrow(transposed))
data2 <- melt(transposed, id.vars = "row")

view(data2)










