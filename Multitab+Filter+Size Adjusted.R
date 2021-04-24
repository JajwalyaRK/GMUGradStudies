library(shiny)
library(plotly)
library(viridis)
library(tidyr)
library(plyr)
library(gridExtra)
require(ggplot2)
require(plyr)
require(dplyr)
library(ggmap)
library(tidyverse)
library(networkD3)
library(igraph)
library(plyr)
#End of Libraries

# Plotting networks in R

# Network graph data pre-proessing

THChinaData = as.data.frame(read.csv("TeaHorseChina.csv", header = TRUE))
cities <- THChinaData %>%
  group_by(NODE1, NODE2, TYPE, EARLYDATE) %>%
  summarize(counts = n()) %>%
  ungroup() %>%
  arrange(desc(counts))

# Data format: dataframe with 3 variables; variables 1 & 2 correspond to interactions; variable 3 is weight of interaction

# The dataset contains Tea and horse trade routes in Tibet, Szechwan and Yunnan c 620-1940 CE. It consists of 68 unique nodes with 71 weighted routes corresponding to 2 connecting nodes.

# The network nodes are characterized by their degree and betweenness centrality. The network edges are characterized by their weight and Dice similarity.


# Create a graph. Use simplyfy to ensure that there are no duplicated edges or self loops

colnames(cities) <- c("SourceName", "TargetName",  "Type","Year","Weight" )
edges <- simplify(graph.data.frame(cities, directed=TRUE))
edges

# networkD3 library requires IDs to start at 0
# Create a node list object (actually a data frame object) that will contain information about nodes

nodes <- data.frame(ID = c(0:(vcount(edges) - 1)), 
                    nName = V(edges)$name)
getNodeID <- function(x){
  which(x == V(edges)$name) - 1 # to ensure that IDs start at 0
}
# Add them to the edge list
citiesEdges <- ddply(cities, 
                     .variables = c("SourceName","TargetName","Year","Weight","Type"),
                     function(x) data.frame(
                       SourceID = getNodeID(x$SourceName),
                       TargetID = getNodeID(x$TargetName)))
citiesID <- citiesEdges %>%
  select(Weight, SourceName,TargetName, Type, Year, SourceID, TargetID)

############################################################################################
# Calculate some node properties and node similarities that will be used to illustrate 
# different plotting abilities and add them to the edge and node lists

# Calculate degree for all nodes

citiesNodes <- cbind(nodes, 
                     nodeDegree=degree(edges, 
                                       v = V(edges), 
                                       mode = "all"))

# Calculate betweenness for all nodes

btw <- betweenness(edges, v = V(edges), 
                   directed = TRUE) / 
  (((vcount(edges) -1) * (vcount(edges) - 2)) /2)
btw.norm <- (btw - min(btw))/(max(btw) - min(btw))
citiesNodes <- cbind(citiesNodes, nodeBetweenness = 100 * btw.norm)
rm(btw, btw.norm)

#Calculate Dice similarities between all pairs of nodes

diceSim <- similarity.dice(edges, vids = V(edges), mode = "all")
F1 <- function(x) {data.frame(diceSim = diceSim[x$SourceID +1, x$TargetID + 1])}
citiesEdges <- ddply(citiesEdges, 
                     .variables = c("SourceName", "TargetName", "Weight","Type","Year"),
                     function(x) data.frame(F1(x)))
rm(diceSim, F1, getNodeID, edges)

############################################################################################
# Create a set of colors for each edge, based on their dice similarity values
# Interpolate edge colors based on the using the "colorRampPalette" function, that 
# returns a function corresponding to a collor palete of "bias" number of elements 
# (total number of edges, i.e., number of rows in the edgeList data frame)


F2 <- colorRampPalette(c("#FF6900", "#694489"), bias = nrow(citiesEdges), space = "rgb", interpolate = "linear")
colCodes <- F2(length(unique(citiesEdges$diceSim)))
edges_col <- sapply(citiesEdges$diceSim, function(x) colCodes[which(sort(unique(citiesEdges$diceSim)) == x)])
rm(colCodes, F2)
citiesEdges <- merge(citiesEdges, citiesID, by = c("Weight","SourceName","TargetName"))
TeaHorseChinaNodes <- write.csv(citiesNodes, file = "TeaHorseChinaNodes.csv")
TeaHorseChinaEdges <- write.csv(citiesEdges, file = "TeaHorseChinaEdges.csv")
ColourScale <- 'd3.scaleOrdinal()
           .range(["#FF6900", "#694489"]);'

# The forceNetwork function creates a network with the following charcateristics:
# Node size as the node betweenness value
# Node color as the node degree
# The distance between two nodes and edge thickness is the edge weight
# Edge color is given by the dice similarity. 
# Each node will be described by its name.

#End of D3

# Interactive map visualization with Scattermapbox to draw lines on the map from NODE1 to NODE2. Note that this particular scattermapbox plot does not require Mapbox access tokens.

# Read data and specify the columns to be used in the map.

caravans <- read.csv("TeaHorseChina.csv")
#head(caravans)
caravans <- caravans %>%
  select("NODE1", "NODE2", "USES", "TYPE", "ROLE", "LONG1", "LAT1", "LONG2", "LAT2",  "DATAID", "EARLYDATE")

caravans$LAT1 <- as.numeric(caravans$LAT1)
caravans$LONG1 <- as.numeric(caravans$LONG1)

caravans$LAT2 <- as.numeric(caravans$LAT2)
caravans$LONG2 <- as.numeric(caravans$LONG2)


# Convert the ROLE to numeric form to get an ordinal value

caravans$ROLE <- as.numeric(caravans$ROLE)

# Specify layout preferences without axes

ax <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE
)
#End of Data for Maps

# Define UI for app

THChinaData1 = as.data.frame(read.csv("AsiaTotal.csv", header = TRUE))
cities1 <- THChinaData1 %>%
  group_by(NODE1, NODE2, TYPE, EARLYDATE) %>%
  summarize(counts = n()) %>%
  ungroup() %>%
  arrange(desc(counts))

# FOr different tabs

colnames(cities1) <- c("SourceName", "TargetName",  "Type","Year","Weight" )
edges1 <- simplify(graph.data.frame(cities1, directed=TRUE))
# networkD3 library requires IDs to start at 0
nodes1 <- data.frame(ID = c(0:(vcount(edges1) - 1)), 
                    nName = V(edges1)$name)
getNodeID <- function(x){
  which(x == V(edges1)$name) - 1 # to ensure that IDs start at 0
}
citiesEdges1 <- ddply(cities1, 
                     .variables = c("SourceName","TargetName","Year","Weight","Type"),
                     function(x) data.frame(
                       SourceID = getNodeID(x$SourceName),
                       TargetID = getNodeID(x$TargetName)))
citiesID1 <- citiesEdges1 %>%
  select(Weight, SourceName,TargetName, Type, Year, SourceID, TargetID)
citiesNodes1 <- cbind(nodes1, 
                     nodeDegree=degree(edges1, 
                                       v = V(edges1), 
                                       mode = "all"))
btw <- betweenness(edges1, v = V(edges1), 
                   directed = TRUE) / 
  (((vcount(edges1) -1) * (vcount(edges1) - 2)) /2)
btw.norm <- (btw - min(btw))/(max(btw) - min(btw))
citiesNodes1 <- cbind(citiesNodes1, nodeBetweenness = 100 * btw.norm)
rm(btw, btw.norm)
diceSim <- similarity.dice(edges1, vids = V(edges1), mode = "all")
F1 <- function(x) {data.frame(diceSim = diceSim[x$SourceID +1, x$TargetID + 1])}
citiesEdges1 <- ddply(citiesEdges1, 
                     .variables = c("SourceName", "TargetName", "Weight","Type","Year"),
                     function(x) data.frame(F1(x)))
rm(diceSim, F1, getNodeID, edges1)
F2 <- colorRampPalette(c("#FFFF00", "#FF0000"), bias = nrow(citiesEdges1), space = "rgb", interpolate = "linear")
colCodes1 <- F2(length(unique(citiesEdges1$diceSim)))
edges_col1 <- sapply(citiesEdges1$diceSim, function(x) colCodes1[which(sort(unique(citiesEdges1$diceSim)) == x)])
rm(colCodes1, F2)
citiesEdges1 <- merge(citiesEdges1, citiesID1, by = c("Weight","SourceName","TargetName"))
TeaHorseChinaNodes1 <- write.csv(citiesNodes1, file = "TeaHorseChinaNodes1.csv")
TeaHorseChinaEdges1 <- write.csv(citiesEdges1, file = "TeaHorseChinaEdges1.csv")
ColourScale <- 'd3.scaleOrdinal()
           .range(["#FF6900", "#694489"]);'
#End of D3

caravans1 <- read.csv("AsiaTotal.csv")
#head(caravans)
caravans1 <- caravans1 %>%
  select("NODE1", "NODE2", "USES", "TYPE", "ROLE", "LONG1", "LAT1", "LONG2", "LAT2",  "DATAID", "EARLYDATE")

caravans1$LAT1 <- as.numeric(caravans1$LAT1)
caravans1$LONG1 <- as.numeric(caravans1$LONG1)

caravans1$LAT2 <- as.numeric(caravans1$LAT2)
caravans1$LONG2 <- as.numeric(caravans1$LONG2)

caravans1$ROLE <- as.numeric(caravans1$ROLE)
ax <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE
)

#End of Tab 2 Data

THChinaData2 = as.data.frame(read.csv("Test.csv", header = TRUE))
cities2 <- THChinaData2 %>%
  group_by(NODE1, NODE2, TYPE, EARLYDATE) %>%
  summarize(counts = n()) %>%
  ungroup() %>%
  arrange(desc(counts))



colnames(cities2) <- c("SourceName", "TargetName",  "Type","Year","Weight" )
edges2 <- simplify(graph.data.frame(cities2, directed=TRUE))
# networkD3 library requires IDs to start at 0
nodes2 <- data.frame(ID = c(0:(vcount(edges2) - 1)), 
                     nName = V(edges2)$name)
getNodeID <- function(x){
  which(x == V(edges2)$name) - 1 # to ensure that IDs start at 0
}
citiesEdges2 <- ddply(cities2, 
                      .variables = c("SourceName","TargetName","Year","Weight","Type"),
                      function(x) data.frame(
                        SourceID = getNodeID(x$SourceName),
                        TargetID = getNodeID(x$TargetName)))
citiesID2 <- citiesEdges2 %>%
select(Weight, SourceName,TargetName, Type, Year, SourceID, TargetID)
citiesNodes2 <- cbind(nodes2, 
                      nodeDegree=degree(edges2, 
                                        v = V(edges2), 
                                        mode = "all"))
btw <- betweenness(edges2, v = V(edges2), 
                   directed = TRUE) / 
  (((vcount(edges2) -1) * (vcount(edges2) - 2)) /2)
btw.norm <- (btw - min(btw))/(max(btw) - min(btw))
citiesNodes2 <- cbind(citiesNodes2, nodeBetweenness = 100 * btw.norm)
rm(btw, btw.norm)
diceSim <- similarity.dice(edges2, vids = V(edges2), mode = "all")
F1 <- function(x) {data.frame(diceSim = diceSim[x$SourceID +1, x$TargetID + 1])}
citiesEdges2 <- ddply(citiesEdges2, 
                      .variables = c("SourceName", "TargetName", "Weight","Type","Year"),
                      function(x) data.frame(F1(x)))
rm(diceSim, F1, getNodeID, edges2)
F2 <- colorRampPalette(c("#FFFF00", "#FF0000"), bias = nrow(citiesEdges2), space = "rgb", interpolate = "linear")
colCodes2 <- F2(length(unique(citiesEdges2$diceSim)))
edges_col2 <- sapply(citiesEdges2$diceSim, function(x) colCodes2[which(sort(unique(citiesEdges2$diceSim)) == x)])
rm(colCodes2, F2)
citiesEdges2 <- merge(citiesEdges2, citiesID1, by = c("Weight","SourceName","TargetName"))
TeaHorseChinaNodes2 <- write.csv(citiesNodes2, file = "TeaHorseChinaNodes2.csv")
TeaHorseChinaEdges2 <- write.csv(citiesEdges2, file = "TeaHorseChinaEdges2.csv")
ColourScale <- 'd3.scaleOrdinal()
           .range(["#FF6900", "#694489"]);'
#End of D3

caravans2 <- read.csv("Test.csv")
#head(caravans)
caravans2 <- caravans2 %>%
  select("NODE1", "NODE2", "USES", "TYPE", "ROLE", "LONG1", "LAT1", "LONG2", "LAT2",  "DATAID", "EARLYDATE")

caravans2$LAT1 <- as.numeric(caravans2$LAT1)
caravans2$LONG1 <- as.numeric(caravans2$LONG1)

caravans2$LAT2 <- as.numeric(caravans2$LAT2)
caravans2$LONG2 <- as.numeric(caravans2$LONG2)

caravans2$ROLE <- as.numeric(caravans2$ROLE)
ax <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE
)

#End of Tab 3 Data


ui <- fillPage(
  tabsetPanel(
  tabPanel('China',fluid = FALSE,
  hr(),
  headerPanel('Tea and horse trade routes in Tibet, Szechwan and Yunnan c. 600 BC - 1940 CE'),
  
  fluidRow(
    column(2, offset = 1,
    selectizeInput(inputId = "TYPE",
                   label = "Select Type",
                   choices = unique(caravans$TYPE),
                   selected = c("Terrain"))),
    br(),
    column(2,
    selectizeInput(inputId = "USES",
                       label = "Select Use",
                       choices = unique(caravans$USES),
                       selected = c("Trade"))),
    ),
  tags$h4("Force-directed Network Analysis of the Silk Route", style="padding:30px", align = "right"),

  mainPanel(fluidRow(
    splitLayout(cellWidths = c("90%", "50%"),   
    plotlyOutput('plot',height = "700px"),
    forceNetworkOutput(outputId = "net")
  )
)
)),
tabPanel('Asia',fluid = FALSE,
         hr(),
         headerPanel('The Silk Road from the Adriatic to the Pacific c. 600 BC - 1940 CE'),
         fluidRow(
           column(2, offset = 1,
            selectizeInput(inputId = "TYPE1",
                          label = "Select Type",
                          choices = unique(caravans1$TYPE),
                          selected = c("Terrestial","Road","River lane", "Sea lane","Canal"),
                          multiple = TRUE)),
           br(),
            column(2,
            selectizeInput(inputId = "USES1",
                               label = "Select Use",
                               choices = unique(caravans1$USES),
                               selected = c("Trade","Messenger","Known", "Business","Religious","Military"),
                                multiple = TRUE)),
   ),
   tags$h4("Force-directed Network Analysis of the Silk Route", style="padding:30px", align = "right"),
         
  mainPanel(fluidRow(
     splitLayout(cellWidths = c("90%", "50%"),   
     plotlyOutput('plot1',height = "700px"), 
     forceNetworkOutput(outputId = "net1")
  )
  )
  )
  ),

tabPanel('Eurasia',fluid = FALSE,
         hr(),
         headerPanel('The Silk Road in Eurasia and Northern Africa c. 600 BCE - 1940 CE'),
         fluidRow(
           column(2, offset = 1,
               selectizeInput(inputId = "TYPE2",
                             label = "Select Type",
                             choices = unique(caravans2$TYPE),
                             selected = c("Terrain","Road","River Lane", "Sea Lane","Canal"),
                             multiple = TRUE)),
               br(),
               column(2,
               selectizeInput(inputId = "USES2",
                                  label = "Select Use",
                                  choices = unique(caravans2$USES),
                                  selected = c("Trade","Messenger","Unknown", "Travel","Exploration", "Pilgrimage","Military"),
                                  multiple = TRUE)),
           ),
         
         tags$h4("Force-directed Network Analysis of the Silk Route",style="padding:30px",  align = "right"),
           
  mainPanel(fluidRow(
      splitLayout(cellWidths = c("90%", "50%"),   
      plotlyOutput('plot2',height = "700px"), 
      forceNetworkOutput(outputId = "net2")
      ))))))
server <- function(input, output){
  
#Tab 1 Begin
  
  output$net <- renderForceNetwork({
    
    EdgesD3 <- subset(citiesEdges,citiesEdges$Type.x %in% input$TYPE|citiesEdges$Type.y %in% input$TYPE)
    #NodesD3 <- subset(citiesNodes, citiesNodes$nName %in% EdgesD3$Sourcename|citiesNodes$nName %in% EdgesD3$TargetName)
    D3_network_LM <- forceNetwork(Links = EdgesD3, # data frame that contains info about edges
                                  Nodes = citiesNodes, # data frame that contains info about nodes
                                  Source = "SourceID", # ID of source node 
                                  Target = "TargetID", # ID of target node
                                  Value = "Weight", # value from the edge list (data frame) that will be used to value/weight relationship amongst nodes
                                  NodeID = "nName", # value from the node list (data frame) that contains node description we want to use (e.g., node name)
                                  Nodesize = "nodeBetweenness",  # value from the node list (data frame) that contains value we want to use for a node size
                                  Group = "nodeDegree",  # value from the node list (data frame) that contains value we want to use for node color
                                  height = 2500, # Size of the plot (vertical)
                                  width = 1500,  # Size of the plot (horizontal)
                                  fontSize = 20, # Font size
                                  linkDistance = JS("function(d) { return 10*d.value; }"), # Function to determine distance between any two nodes, uses variables already defined in forceNetwork function (not variables from a data frame)
                                  linkWidth = JS("function(d) { return d.value/5; }"),# Function to determine link/edge thickness, uses variables already defined in forceNetwork function (not variables from a data frame)
                                  opacity = 1.90, # opacity
                                  arrow = TRUE,
                                  zoom = TRUE, # ability to zoom when click on the node
                                  
                                  opacityNoHover = 0.2,
                                  colourScale = JS(ColourScale)) %>% # opacity of labels when static
    #,linkColour = edges_col) # edge colors 
    htmlwidgets::prependContent(htmltools::tags$h4("Force-directed Network Analysis of the Silk Route", align = "center"))
  })
  
  output$plot <- renderPlotly({
     
    figMB <- filter(caravans,caravans$TYPE %in% input$TYPE & caravans$USES %in% input$USES)
    #print (figMB)
    figMB <- plot_ly(
      data = figMB,
      lat = ~LAT1,
      lon = ~LONG1,
      mode = 'markers+lines',
      #size = ~ROLE,
      sizes = c(5, 20),
      marker = list(sizemode = 'diameter'),
      color = ~USES,
      #size = ~ROLE,
      colors = '#e41a1c',
      opacity = 1.5,
      showlegend = FALSE,
      type = "scattermapbox")
    
    figMB <- figMB %>%
      add_trace(
        type = 'scattermapbox',
        mode = "markers+lines",
        x = ~LONG2,
        y = ~LAT2,
        hoverinfo = 'text',
        text = ~paste('</br><b>From</b>', NODE1, '</br><b>to</b> ', NODE2, '</br><b>on</b> ', TYPE),
        showlegend = TRUE) 
    
    figMB <- figMB %>%
      add_markers(
        type = 'scattermapbox',
        mode= "markers",
        x = ~LONG1, y = ~LAT1,
        alpha = 0.8,
        showlegend = FALSE
      )
    
    figMB <- figMB %>%
      layout(
        xaxis = ax, yaxis = ax,
        title = 'Mapping the Ancient Silk Road (Hover for details)',
        mapbox = list(
          style = "stamen-terrain",
          #marker = list(
          #  line = list(dash = "solid", width = "1", opacity = 0.1)), 
          legend= list(itemsizing='constant'),
          zoom = 4,
          center = list(lon = 94, lat= 27),
          margin =list(l=0,t=0,b=0,r=0)))
  })
  
#Tab 1 End
  

#Tab 2 Begin
  output$net1 <- renderForceNetwork({
    
    EdgesD31 <- subset(citiesEdges1,citiesEdges1$Type.x %in% input$TYPE1|citiesEdges1$Type.y %in% input$TYPE1)
    #NodesD3 <- subset(citiesNodes, citiesNodes$nName %in% EdgesD3$Sourcename|citiesNodes$nName %in% EdgesD3$TargetName)
    D3_network_LM1 <- forceNetwork(Links = EdgesD31, # data frame that contains info about edges
                                  Nodes = citiesNodes1, # data frame that contains info about nodes
                                  Source = "SourceID", # ID of source node 
                                  Target = "TargetID", # ID of target node
                                  Value = "Weight", # value from the edge list (data frame) that will be used to value/weight relationship amongst nodes
                                  NodeID = "nName", # value from the node list (data frame) that contains node description we want to use (e.g., node name)
                                  Nodesize = "nodeBetweenness",  # value from the node list (data frame) that contains value we want to use for a node size
                                  Group = "nodeDegree",  # value from the node list (data frame) that contains value we want to use for node color
                                  height = 2500, # Size of the plot (vertical)
                                  width = 1500,  # Size of the plot (horizontal)
                                  fontSize = 20, # Font size
                                  linkDistance = JS("function(d) { return 10*d.value; }"), # Function to determine distance between any two nodes, uses variables already defined in forceNetwork function (not variables from a data frame)
                                  linkWidth = JS("function(d) { return d.value/5; }"),# Function to determine link/edge thickness, uses variables already defined in forceNetwork function (not variables from a data frame)
                                  opacity = 1.90, # opacity
                                  arrow = TRUE,
                                  zoom = TRUE, # ability to zoom when click on the node
                                  
                                  opacityNoHover = 0.2,
                                  colourScale = JS(ColourScale)) %>% # opacity of labels when static
      #,linkColour = edges_col) # edge colors 
      htmlwidgets::prependContent(htmltools::tags$h4("Force-directed Network Analysis of the Silk Route", align = "center"))
  })
  output$plot1 <- renderPlotly({
    figMB1 <- filter(caravans1,caravans1$TYPE %in% input$TYPE1 & caravans1$USES %in% input$USES1)
    #print (figMB)
    figMB1 <- plot_ly(
      data = figMB1,
      lat = ~LAT1,
      lon = ~LONG1,
      mode = 'markers+lines',
      #size = ~ROLE,
      marker = list(sizemode = 'diameter'),
      color = ~USES,
      opacity = 1.5,
      showlegend = FALSE,
      type = "scattermapbox")
    
    figMB1 <- figMB1 %>%
      add_trace(
        type = 'scattermapbox',
        mode = "markers+lines",
        x = ~LONG2,
        y = ~LAT2,
        hoverinfo = 'text',
        text = ~paste('</br><b>From</b>', NODE1, '</br><b>to</b> ', NODE2, '</br><b>on</b> ', TYPE),
        showlegend = TRUE) 
    
    figMB1 <- figMB1 %>%
      add_markers(
        type = 'scattermapbox',
        mode= "markers",
        x = ~LONG1, y = ~LAT1,
        alpha = 0.5,
        showlegend = FALSE
      )
    
    figMB1 <- figMB1 %>%
      layout(
        xaxis = ax, yaxis = ax,
        title = 'Mapping the Ancient Silk Road (Hover for details)',
        mapbox = list(
          style = "stamen-terrain",
          #marker = list(
          #  line = list(dash = "solid", width = "1", opacity = 0.1)), 
          legend= list(itemsizing='constant'),
          zoom = 3,
          center = list(lon = 55, lat= 36),
          margin =list(l=0,t=0,b=0,r=0)))
  })

#Tab 2 End
  
#Tab 3 Begin
  output$net2 <- renderForceNetwork({
    
    EdgesD32 <- subset(citiesEdges2,citiesEdges2$Type.x %in% input$TYPE2|citiesEdges2$Type.y %in% input$TYPE2)
    #NodesD3 <- subset(citiesNodes, citiesNodes$nName %in% EdgesD3$Sourcename|citiesNodes$nName %in% EdgesD3$TargetName)
    D3_network_LM2 <- forceNetwork(Links = EdgesD32, # data frame that contains info about edges
                                   Nodes = citiesNodes2, # data frame that contains info about nodes
                                   Source = "SourceID", # ID of source node 
                                   Target = "TargetID", # ID of target node
                                   Value = "Weight", # value from the edge list (data frame) that will be used to value/weight relationship amongst nodes
                                   NodeID = "nName", # value from the node list (data frame) that contains node description we want to use (e.g., node name)
                                   Nodesize = "nodeBetweenness",  # value from the node list (data frame) that contains value we want to use for a node size
                                   Group = "nodeDegree",  # value from the node list (data frame) that contains value we want to use for node color
                                   height = 2500, # Size of the plot (vertical)
                                   width = 1500,  # Size of the plot (horizontal)
                                   fontSize = 20, # Font size
                                   linkDistance = JS("function(d) { return 10*d.value; }"), # Function to determine distance between any two nodes, uses variables already defined in forceNetwork function (not variables from a data frame)
                                   linkWidth = JS("function(d) { return d.value/5; }"),# Function to determine link/edge thickness, uses variables already defined in forceNetwork function (not variables from a data frame)
                                   opacity = 1.90, # opacity
                                   arrow = TRUE,
                                   zoom = TRUE, # ability to zoom when click on the node
                                   
                                   opacityNoHover = 0.2,
                                   colourScale = JS(ColourScale)) %>% # opacity of labels when static
      #,linkColour = edges_col) # edge colors 
      htmlwidgets::prependContent(htmltools::tags$h4("Force-directed Network Analysis of the Silk Route", align = "center"))
  })
  
  output$plot2 <- renderPlotly({
    figMB2 <- filter(caravans2,caravans2$TYPE %in% input$TYPE2 & caravans2$USES %in% input$USES2)
    #print (figMB)
    figMB2 <- plot_ly(
      data = figMB2,
      lat = ~LAT1,
      lon = ~LONG1,
      mode = 'markers+lines',
      #size = ~ROLE,
      marker = list(sizemode = 'diameter'),
      color = ~USES,
      opacity = 1.5,
      showlegend = FALSE,
      type = "scattermapbox")
    
    figMB2 <- figMB2 %>%
      add_trace(
        type = 'scattermapbox',
        mode = "markers+lines",
        x = ~LONG2,
        y = ~LAT2,
        hoverinfo = 'text',
        text = ~paste('</br><b>From</b>', NODE1, '</br><b>to</b> ', NODE2, '</br><b>on</b> ', TYPE),
        showlegend = TRUE) 
    
    figMB2 <- figMB2 %>%
      add_markers(
        type = 'scattermapbox',
        mode= "markers",
        x = ~LONG1, y = ~LAT1,
        alpha = 0.5,
        showlegend = FALSE
      )
    
    figMB2 <- figMB2 %>%
      layout(
        xaxis = ax, yaxis = ax,
        title = 'Mapping the Ancient Silk Road (Hover for details)',
        mapbox = list(
          style = "stamen-terrain",
          #marker = list(
          #  line = list(dash = "solid", width = "1", opacity = 0.1)), 
          legend= list(itemsizing='constant'),
          zoom = 3,
          center = list(lon = 55, lat= 36),
          margin =list(l=0,t=0,b=0,r=0)))
  })
  
  #Tab 3 End
}
shinyApp(ui,server)
