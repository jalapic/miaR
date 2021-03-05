
### Introduction to Writing Functions

if (!require("babynames")) install.packages("babynames")
library(babynames)
library(tidyverse)


#### Basic Overview

# this is the basic template of a function

name_of_function <- function(x){
  # do stuff in here, e.g.
  vals <- sum(x)
  return(vals)
}


# let's write a function to get the mean of numbers

x <- runif(10)
x
sum(x)/length(x)

mean_x <- function(x){
  sum(x)/length(x)
} 

mean_x(x = runif(100))
mean_x(x = 1:15)
mean_x(x = c(3,4,6,10,15))
# functions return the last thing you calculated
# that is not stored as an object
# e.g. this won't work....

mean_x1 <- function(x){
  res <- sum(x)/length(x)
} 

mean_x1(x = c(3,4,6,10,15))

# instead tell it precisely what to return:
mean_x2 <- function(x){
  res <- sum(x)/length(x)
  return(res)
} 

mean_x2(x = c(3,4,6,10,15))




### Vaguely more interesting example:

## e.g. two parameters....

# let's say we want to create a random ID
# the ID should contain three letters and then three numbers

LETTERS # all the letters
LETTERS[4] # returns 4th letter
LETTERS[1:10] # returns the first 10 letters

sample(LETTERS,3,T)
sample(0:9,3,T)

paste0(sample(LETTERS,3,T), sample(0:9,3,T) )

v1 <- sample(LETTERS,3,T)
v2 <- sample(0:9,3,T)

v1
v2

id <- paste(c(v1,v2),collapse="")


# create a function to do this:

id_maker <- function(nums = 0:9){

  v1 <- sample(LETTERS,3,T)
  v2 <- sample(nums,3,T)
  id <- paste(c(v1,v2),collapse="")
  return(id)  
}

id_maker()

id_maker(nums = 1:9)
id_maker(nums = 5:9)


# say we didn't want letters to repeat ever
id_maker <- function(nums = 0:9, replet = T){
  
  v1 <- sample(LETTERS,3,replet)
  v2 <- sample(nums,3,T)
  id <- paste(c(v1,v2),collapse="")
  return(id)  
}

id_maker(replet = F)


# allow users to determine how many numbers/letters

id_maker <- function(nums = 0:9, replet = T, no_let=3, no_num=3){
  
  v1 <- sample(LETTERS,no_let,replet)
  v2 <- sample(nums,no_num,T)
  id <- paste(c(v1,v2),collapse="")
  return(id)  
}

id_maker()

id_maker(no_let = 2, no_num = 5)





###







#### Example with real data

head(babynames)
tail(babynames)


# e.g. let's make a graph of Mia over time.

ggplot(babynames %>% filter(name=="Mia"), 
       aes(x=year, y=n, color=sex))+
  geom_line() +
  ggtitle("Mia")

# redo but for Violet
ggplot(babynames %>% filter(name=="Violet"), 
       aes(x=year, y=n, color=sex))+
  geom_line() +
  ggtitle("Violet")

# it gets annoying to do this for every name we're interested in
ggplot(babynames %>% filter(name=="Emma"), 
       aes(x=year, y=n, color=sex))+
  geom_line() +
  ggtitle("Emma")


gg_name <- function(idname = "Mia"){

  p <-  ggplot(babynames %>% filter(name== idname), 
         aes(x=year, y=n, color=sex))+
    geom_line() +
    ggtitle(idname)
    
  return(p)
}

gg_name()
gg_name(idname="Violet")
gg_name(idname="Emma")
gg_name(idname="Skylar")
gg_name(idname="Tyler")

# any ggplot code can be added
gg_name(idname="Tyler") +
  xlab("Year") + ylab("Frequency")
  


# let's say you want users to dictate colors:

gg_name <- function(idname = "Mia", color1="blue", color2="orange"){
  
  p <-  ggplot(babynames %>% filter(name== idname), 
               aes(x=year, y=n, color=sex))+
    geom_line() +
    ggtitle(idname) +
    scale_color_manual(values=c(color1,color2))
  
  return(p)
}

gg_name()
gg_name(color2="darkseagreen")

gg_name(color2="darkseagreen") + theme_classic()
