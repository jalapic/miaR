library(tidyverse)
library(dplyr)
library(plyr)
library(ggplot2)
library(lubridate)

df <- read_csv("data/austin_animal.csv")
head(df)

as.data.frame(df[1:4,])
str(df)
table(df$"Animal Type")
length(df$Name)
count(df[!is.na(df$Name),])


### Playing around with names.....
## get rid of *, weights

df$Name
df$Name <- tolower(df$Name)
df$Name <- gsub("\\*", "", df$Name) 
df$Name <- gsub("[0-9]+", "", df$Name)
df$Name <- gsub("grams", "", df$Name)
df$Name

## What is the most common cat name?
## 46,367 cats

cats <- df %>% filter(`Animal Type` == "Cat")
head(cats)
nrow(cats)

count(cats, "Name") %>%
  arrange(desc(freq)) %>%
  head(20)

## Most common names:
## (1) Luna, 122
## (2) Charlie, 119
## (3) Lucy, 84
## (4) Bella, 83
## (5) Max, 83

## (18) Mia, 60 


## What is the most common dog name?
## 70,184 dogs

dogs <- df %>% filter(`Animal Type` == "Dog")
head(dogs)
nrow(dogs)  

count(dogs, "Name") %>%
  arrange(desc(freq)) %>%
  head(20)

## (1) Max, 538
## (2) Bella, 472
## (3) Luna, 413
## (4) Rocky, 373
## (5) Daisy, 362

## Conclusion for cat and dog names:
## Luna, Max, and Bella are both within the top 5 most common names for both cats and dogs



## What is the most common day for animals to be dropped off?
# bar graph

## What is the most common time of day for animals to be dropped off?
# histogram or density plot


# some things to look up
# lubridate package has functions to extract times and dates.
# as.Date() converts to a date
# strptime gives you the codes to use



df$DateTime
df$Date <- sub()


df$DateTime

df$Time <- sub("^\\S+\\s+", '', df$DateTime)
df$Time

hours <- sub("\\:.*", "", df$Time) 

count(hours)

times <- table(hours)

times

plot(times, xlab = "time of day (hours)", ylab = "amount of intakes", main = "when are animals dropped off?")


## Conclusions for time of day
# 11am is the most common drop-off time (trend carries on into early afternoon)

df$Date <- as.Date(df$DateTime,format = "%m/%d/%Y")
df$Date
lubridate::wday(df$Date)

## 1 = Sunday

days <- count(wday(df$Date, label = TRUE, abbr = FALSE))
days

ggplot(days, aes(x, freq)) +
  geom_col(fill = "lightblue", color = "navy", show.legend = FALSE) +
  xlab("days of the week") +
  ylab("amount of intakes") +
  ylim(0, 20000)


## Conclusions for days of the week
# Pretty evident that AAC receives the fewest intakes on Sunday, but rest of data is comparable



## AAS open every day 11am - 6pm
## New task: turn addresses into coordinates and plot on a map

colnames(df) <- c("id", "name", "datetime", "monthyear", "location", 
                  "type", "condition", "animal", "sex", "age", "breed", "color", "time", "date")
colnames(df)


## working with OpenStreetMap

if(!require("osmdata")) install.packages("osmdata")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("sf")) install.packages("sf")
if(!require("ggmap")) install.packages("ggmap")

library(tidyverse)
library(osmdata)
library(sf)
library(ggmap)


available_features()
available_tags("amenity")

q <- getbb("Austin") %>%
  opq() %>%
  add_osm_feature("amenity", "fuel")
str(q)

fuel <- osmdata_sf(q)

map <- get_map(getbb("Austin"), maptype = "roadmap")

ggmap(map) +
  geom_sf(data = fuel$osm_points,
          inherit.aes = FALSE,
          color = "darkblue",
          fill = "darkblue",
          alpha = .5,
          size = 0.5,
          shape = 21)

map1 <- get_map(getbb("Austin"), maptype = "roadmap", zoom = 13, source = "osm")

ggmap(map1) +
  geom_point(data = ##__, 
             aes(x = Longitude, y = Latitude), 
             color = "darkblue", 
             size = 1.5,
             alpha = 0.5)


## how do I use AAC data?
## Let's try tidygeocoder

## step 1: install tidygeocoder through `packages` tab
library(tidygeocoder)
library(ggmap)
library(data.table)
library(sp)
library(rgdal)

## step 2: filter out dogs -> filter out dogs who have names -> only retrieve `location` column
dogs <- df %>% filter(animal == "Dog")
dogs <- (dogs[!is.na(dogs$name),])
dogs <- dogs[,5]
dogs <- as.data.frame(dogs)


## step 3: find coordinates

addr <- count(dogs) %>%
  arrange(desc(freq))

head(addr, 60)

df <- geo(street = "7201 Levander Loop", 
          city = "Austin",
          state = "Texas", 
          method = "census")
df

adds <- data.frame(
  street = c("7201 Levander Loop", "4434 Frontier Trail", "1156 W Cesar Chavez", "12034 Research Blvd",
             "4106 N Lamar Blvd", "600 Barwood Park", "12138 N Lamar Blvd", "5701 Johnny Morris Rd", 
             "400 Grove Blvd", "5106 Village Square", "5801 Ainez Dr", "5200 Knight Cir", 
             "13021 Dessau Rd", "2100 Barton Springs Rd", "1300 Crossing Pl", "1800 E Stassney L",
             "12118 Walnut Park Crossing", "1700 Teri Rd", "25613 River Fern Ct", "7601 Daffan Ln" ),
  city = c("Austin","Austin","Austin", "Austin", "Austin", "Austin","Austin",
           "Austin", "Austin", "Austin", "Austin", "Austin", "Austin", "Austin", "Austin",
           "Austin", "Austin", "Austin", "Austin", "Austin"),
  state = c("Texas","Texas","Texas", "Texas", "Texas", "Texas", "Texas", "Texas", "Texas", "Texas",
            "Texas", "Texas", "Texas", "Texas", "Texas", "Texas", "Texas", "Texas", "Texas", "Texas")
  
)

adds

## for loops time

results <- vector('list', nrow(adds))

for(i in 1:nrow(adds)){
  
  results[[i]] <- geo(street = adds[i,1], 
                      city = adds[i,2],
                      state = adds[i,3], 
                      method = "census")
  
}

results

do.call("rbind", results)
list <- do.call("rbind", results)
list
list <- (list[!is.na(list$lat),])
list
list = as.data.table(list)
coordinates(list) = c("lat", "long")
points <- coordinates(list)
points = as.data.frame(points)
points

map1 <- get_map(getbb("Austin"), maptype = "roadmap")

ggmap(map1) +
  geom_point(data = points,
               aes(x = long, y = lat), 
             color = "darkblue", 
             size = 1.5,
             alpha = 0.5)









