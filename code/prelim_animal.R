### First look at Austin Animal Data

library(tidyverse)

df <- read_csv("data/austin_animal.csv")

head(df)
as.data.frame(df[1:4,])

str(df)

### Playing around with names.....

df$Name

# let's get rid of stars...

gsub("\\*", "", df$Name)

# work out how to remove anything with "Grams" in the name.


# what's the most popular names?  (for cats, for dogs, by sex, by age ?)

# Extend this code to find any animal with certain patterns:

df$name <- gsub("\\*", "", df$Name)

# let's make everything lowercase

df$name <- tolower(df$name)
df$name

# search for a string match
grepl("blue", df$name)

df$name[grepl("blue", df$name)]

df[grepl("blue", df$name),]


###  Draw a graph (histogram/density type thing) 
# showing what times of day animals get dropped off
# do the same thing for days of the week (maybe this would be a bar graph?)

# some things to look up
# lubridate package has functions to extract times and dates.
# as.Date() converts to a date
# strptime gives you the codes to use

x <- df[1:3,3]  # just grab 3 dates and times.
as.Date(x, format = "%m/%d/%Y %H:%M")



as.Date("4/14/2016 18:43", format = "%m/%d/%Y %H:%M")

as.Date(df$DateTime, format = "%m/%d/%Y %H:%M")

lubridate::wday("2018-07-12")

lubridate::wday(as.Date(df$DateTime, format = "%m/%d/%Y %H:%M"))

# splitting info:

as.data.frame(df[1:4,])

df$times <- sub("^\\S+\\s+", '', df$DateTime)

# remember if you have a number as a character e.g. "12" , "5", you can convert to number like this:
# as.numeric(x)

