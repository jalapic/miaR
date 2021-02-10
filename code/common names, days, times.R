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
  geom_col(fill = "gray", color = "black", show.legend = FALSE) +
  xlab("days of the week") +
  ylab("amount of intakes") +
  ylim(0, 20000)


## Conclusions for days of the week
# Pretty evident that AAC receives the fewest intakes on Sunday, but rest of data is comparable

