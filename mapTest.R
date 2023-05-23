library(leaflet)
library(maps)
library(tidygeocoder)
library(dplyr)
library(htmltools)
library(htmlwidgets)
library(tigris)

names <- c("Reagan Gerena","Rachel Rockwell","Brian Hogan","Nate Townsend","Julia Smith","Steve Mallory", 
"Mao Own, Annie Dimmock, and Chin Chukwuani","Jenna Outlet and Pat Higlemire","Stephanie Kessler", 
"Matt Kelley","Hanna Lee-Kleb","Derek Larabee","Jake Pacheco","Mark Sanders","Laura Carr", 
"Kameron Hurley","Sarah Butler","Tony Gianneschi","Dowell Harmon","Gavin Sipes","Sarah Brabec", 
"Ariel Kepler-Wolfe","Christine Sedlacek","Chicago Team Cluster","Chris Witt","Johnna Decker", 
"Kira Brabeck","Cedar Falls Office","Mason Widmer","Jacob Runia","DSM Office", 
"Jeff Unseld and Emily Carballo","Megan Hoover","Denver Team ","Cade Turner", 
"Nadia Hamden","Taylor Boyenga","Madison Barker and Wes Seay","Patrick Istvan","Faye Haun", 
"Taylor Fisher","Divya Bisht, Eric Siegel, and Natalie Holder","Amanda Zindel", 
"Mike Armijo and Mathew Zannoni","Fahad Mahood and Josh Watkins","Brooke Vasey","Janie Kate Jordan", 
"Tampa Office")

visitCity <- c("Coconut Creek","Lake Worth","Winter Park","Green Mountain","Henrico","Silver Spring",
               "Philadelphia","Hartford","Franklin","Worcester","Traverse City","Grand Rapids","Bowne Township",
               "Rochester","Temperance","Dayton","West Chester","Lexington","Danville","Vine Grove",
               "Lafayette","Fort Wayne","Harbert","Chicago","Madison","Bernard","Eldridge","Cedar Falls",
               "Lakeville","Harrisburg","Clive","Kansas City","Lawrence","Denver","Herriman","Logan",
               "Kalispell","Tacoma","Longview","Sacramento","Tehachapi","Los Angeles",
               "Tucson","Albuquerque","Fort Worth","Austin","Bryan","Tampa")


statesAbbr <- c("FL","FL","FL","NC","VA","MD","PA","CT","MA",
                "MA","MI","MI","MI","MI","MI","OH","OH","KY",
                "KY","KY","IN","IN","MI","IL","WI","IA","IA","IA",
                "MN","SD","IA","MO","KS","CO","UT","UT","MT","WA",
                "WA","CA","CA","CA","AZ","NM","TX","TX","TX","FL")

visitedData <- data.frame(names,visitCity,statesAbbr)

visitedData <- visitedData %>%
  tidygeocoder::geocode(city = visitCity, state = statesAbbr) %>%
  filter(!is.na(lat)) 

visitedData <- visitedData %>% 
  mutate(visitedTF = FALSE)


content <- paste0(
  visitedData$names, "<br/>", visitedData$visitCity,", ", visitedData$statesAbbr) %>%
  lapply(htmltools::HTML)

mapStates = map("state", fill = TRUE, plot = FALSE)

  mileage <- c(31, 130, 511, 270, 85, 103, 270, 44, 20, 684, 122, 10, 86, 56, 134, 15, 85, 25, 63, 
               223, 113, 123, 76, 147, 108, 50, 135, 190, 198, 240, 315, 40, 540, 514, 93, 570, 528, 
               97, 627, 317, 100, 380, 350, 522, 89, 94, 1075)
  
  day <- 1
  visitedData[c(1:day),"visitedTF"] <- TRUE
  
  spinUtechIcon <- icons(
    iconUrl = ifelse(visitedData$visitedTF == TRUE,
                     "https://i.ibb.co/7yrQyby/spinMap.png",
                     "https://i.ibb.co/7jyntvj/spinicongreypng.png"
    ),
    iconWidth = 22, iconHeight = 22,
    iconAnchorX = 0, iconAnchorY = 0,
  )
  
  spinUTrekMap <- leaflet(data = mapStates) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(fillColor = "white", weight=0.1, color = "black", stroke = TRUE) %>%
  addMarkers(visitedData$long, visitedData$lat, icon=spinUtechIcon,popup = ~content, label =  ~content) %>%
    addPolylines(as.numeric(c(visitedData[1,c("long")],visitedData[2,c("long")])),
                 as.numeric(c(visitedData[1,c("lat")],visitedData[2,c("lat")])), color="#3c7f8d",
                 label = ~paste("Miles:", mileage[day]),
                 labelOptions = labelOptions(noHide = T,minZoom = 10, maxZoom = 10)
                 ) #%>%
    # addPolylines(as.numeric(c(visitedData[2,c("long")],visitedData[3,c("long")])),
    #              as.numeric(c(visitedData[2,c("lat")],visitedData[3,c("lat")]))) %>%
    # addPolylines(as.numeric(c(visitedData[3,c("long")],visitedData[4,c("long")])),
    #              as.numeric(c(visitedData[3,c("lat")],visitedData[4,c("lat")])))
    # 
  spinUTrekMap
  
  saveWidget(spinUTrekMap, file="/Users/ericsiegel/desktop/spinUTrekMap/spinUTrekMap.html", title = "SpinUTrek Map")
  
  
  

  
  

  