# How to plot an interative world map by Jewel Johnson
# Link to the blog post: https://jeweljohnsonj.github.io/jeweljohnson.github.io/posts/2022-01-17-the-world-happiness-report-2021/

# Download the zip file containing shape file. A .shp file is a vector storage format for geographic location
# Please change the 'destfile' location to where your zip file is downloaded
download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="shp/world_shape_file.zip")
# You now have it in your current working directory. Mine I placed it in a sub folder called 'shp' in my working directory

# Unzip this file into shp directory. You can do it with R (as below).
unzip("shp/world_shape_file.zip", exdir = "shp/")
# Your .shp file is this --> TM_WORLD_BORDERS_SIMPL-0.3.shp

# Read this shape file with the sf library.
# since {rgdal} is retiring I am using {sf}

#install.packages("sf")
library(sf)
world_spdf <- st_read(paste0(getwd(),"/shp/TM_WORLD_BORDERS_SIMPL-0.3.shp"), stringsAsFactors = FALSE)

# load the dataset which you have downloaded
# please change the location accordingly
hap_pre <- read.csv(paste0(getwd(),"/datasets/world-happiness-report-2021.csv"))

# renaming column names of ease of use
colnames(hap_pre)[1] <- "country"
colnames(hap_pre)[3] <- "score"

library(dplyr)
# selecting country and score columns
hap <- hap_pre %>% select(country,score)
# assigning ranks based on ladder score
hap <- hap %>% mutate(rank = rank(-score))

# checking which country names are a mismatch between map data and the downloaded dataset
# this is an important check as we have to join the happiness dataset and shp file with country names
anti_join(hap, world_spdf,  by = c("country" = "NAME"))

# correcting country names, note that some countries are not available in the shp file
# the list below are the ones that are available in the shp file
correct_names <- c("Taiwan Province of China" = "Taiwan",
                   "South Korea" = "Korea, Republic of",
                   "Moldova"  = "Republic of Moldova",
                   "Hong Kong S.A.R. of China"= "Hong Kong",
                   "Vietnam" = "Viet Nam",
                   "Congo (Brazzaville)" = "Congo",
                   "Laos" = "Lao People's Democratic Republic",
                   "Iran" = "Iran (Islamic Republic of)",
                   "Palestinian Territories" = "Palestine",
                   "Myanmar" = "Burma",
                   "Tanzania" = "United Republic of Tanzania")

# recoding country names 
hap2 <- hap %>% mutate(country = recode(country, !!!correct_names))

# the command below shows which countries are not joined
# unfortunately we cannot add these countries unless we change the shp file
# for now let us omit these countries
anti_join(hap2, world_spdf,  by = c("country" = "NAME"))

# joining shp file and the happiness data
world_hap <-  left_join(world_spdf, hap2, by = c("NAME" = "country"))

#install.packages("leaflet")
library(leaflet)
fill_col <- colorNumeric(palette="viridis", domain=world_hap$score, na.color="transparent")

# Prepare the text for tooltips:
text <- paste(
  "Country: ", world_hap$NAME,"<br/>", 
  "Score: ", world_hap$score, "<br/>", 
  "Rank: ", world_hap$rank, 
  sep="") %>%
  lapply(htmltools::HTML)

# plotting interactive map
leaflet(world_hap) %>% 
  addTiles()  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( 
    fillColor = ~fill_col(score), 
    stroke=TRUE, 
    fillOpacity = 0.9, 
    color= "grey", 
    weight=0.3,
    label = text,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  ) %>%
  addLegend( pal=fill_col, values=~score, opacity=0.7, title = "Score", position = "bottomleft" )
