### First look at Austin Animal Data

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
## get rid of *, NA, weights

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

x <- df[1:3,3]  # just grab 3 dates and times.
as.Date(x, format = "%m/%d/%Y %H:%M")

df$DateTime
df$Date <- sub()


df$DateTime
df$Time <- sub("^\\S+\\s+", '', df$DateTime)
df$Time







as.Date("4/14/2016 18:43", format = "%m/%d/%Y %H:%M")

as.Date(df$DateTime, format = "%m/%d/%Y %H:%M")

lubridate::wday("2018-07-12")

lubridate::wday(as.Date(df$DateTime, format = "%m/%d/%Y %H:%M"))



# splitting info:

as.data.frame(df[1:4,])

df$times <- sub("^\\S+\\s+", '', df$DateTime)

# remember if you have a number as a character e.g. "12" , "5", you can convert to number like this:
# as.numeric(x)

