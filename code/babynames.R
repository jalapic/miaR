## LOAD IN PACKAGES
library(tidyverse)
library(gridExtra)
library(ggplot2)

## LOAD IN DATA
load("../data/babynames.rda")

## MAKE A FUNCTION TO PLOT NAMES
gg_name_line <- function(idname = NULL, color1 = NULL, color2 = NULL) {
  p1 <- ggplot(babynames %>% filter(name==idname),
              aes(x=year, y=n, color=sex)) +
    geom_line(color1="hotpink", color1="darkblue") +
    ggtitle(idname) +
    scale_color_manual(values=c(color1,color2))
  return(p1)
}

grid.arrange(
  gg_name_line("Mia", color1 = "hotpink", color2 = "darkblue"),
  gg_name_line("James", color1 = "hotpink", color2 = "darkblue"),
  gg_name_line("Jolie", color1 = "hotpink", color2 = "darkblue"),
  gg_name_line("Tyler", color1 = "hotpink", color2 = "darkblue"),
  gg_name_line("Maddie", color1 = "hotpink", color2 = "darkblue")
)

## MAKING A FUNCTION TO PLOT OUR NAMES
our_names <- c("Mia", "James", "Tyler", "Jolie", "Maddie")

z <- babynames %>%
  filter(name %in% our_names)

ggplot(z, aes(x=reorder(name, n), y=n, fill=sex)) + 
  geom_col() +
  scale_y_continuous(labels=function(x) format(x, scientific=FALSE)) +
  ggtitle("sex distribution of our names") +
  xlab("name") +
  ylab("total")


gg_name_col <- function(id2_name = NULL, fill1 = NULL, fill2 = NULL) {
  p3 <- ggplot(babynames %>% filter(name==id2_name),
              aes(x=id2_name, y=n, fill=sex)) +
    geom_col(position = "dodge") +
    ggtitle(id2_name) +
    scale_color_manual(values=c(fill1,fill2)) +
    xlab("") +
    ylab("total") +
    scale_y_continuous(labels=function(x) format(x, scientific=FALSE))
}

grid.arrange(
  gg_name_col("Mia", fill1 = "hotpink", fill2 = "darkblue"),
  gg_name_col("James", fill1 = "hotpink", fill2 = "darkblue"),
  gg_name_col("Jolie", fill1 = "hotpink", fill2 = "darkblue"),
  gg_name_col("Tyler", fill1 = "hotpink", fill2 = "darkblue"),
  gg_name_col("Maddie", fill1 = "hotpink", fill2 = "darkblue")
)


## TURN INTO WIDE DF TO CREATE SEX RATIOS AND FIND UNISEX NAMES
# SPECIAL CRITERIA: year 2002
df <- babynames %>%
  filter(year==2002) %>%
  group_by(name, sex) %>%
  summarize(total = sum(n)) %>%
  pivot_wider(
    names_from = sex, values_from = total, values_fill = 0)

colnames(df) <- c("name", "nM", "nF")

df <- df %>% mutate(ratio = nM/(nM + nF), total = nM + nF)

unisex_names <- df %>%
  filter(ratio > .33, ratio < .67, total > 250)

unisex_names <- c("Amari", "Armani", "Austyn", "Avery", "Baby", "Campbell",
                  "Casey", "Devyn", "Elisha", "Emerson", "Harley", "Jackie",
                  "Jaiden", "Jessie", "Joan", "Jordan", "Jorden", "Justice",
                  "Kerry", "London", "Loren", "Mckinley", "Payton", "Peyton",
                  "Reese", "Reilly", "Riley", "Rory", "Rowan", "Ryley",
                  "Sage", "Shea", "Skyler")

p4 <- babynames %>%
  filter(name %in% unisex_names, year==2002)

ggplot(p3, aes(x=name, y=n, fill=sex)) +
  geom_col() +
  coord_flip() +
  ylab("total") +
  ggtitle("unisex names in 2002")

## _____________________________________________________________________________

## EMD STUFF

grid.arrange(
  gg_name_line("Frankie", color1 = "hotpink", color2 = "darkblue"),
  gg_name_line("Marion", color1 = "hotpink", color2 = "darkblue"),
  gg_name_line("Peyton", color1 = "hotpink", color2 = "darkblue"),
  gg_name_line("Chris", color1 = "hotpink", color2 = "darkblue"),
  gg_name_line("John", color1 = "hotpink", color2 = "darkblue")
)

bn <- babynames %>%
  group_by(year, name, sex) %>%
  summarize(total = sum(n)) %>%
  pivot_wider(names_from = sex, values_from = total, values_fill = 0) %>%
  mutate(ratio = M/(M+`F`), total = M + `F`)

colnames(bn) <- c("year", "name", "nM", "nF", "ratio", "total")

bnames <- unique(bn$name)
## 97, 310 unique babynames

gg_name_area <- function(id3_name=NULL) {
  ggplot(babynames %>% filter(name==id3_name),
         aes(x=year, y=n, fill=sex)) +
    geom_area() +
    ylab("total") +
    ggtitle(id3_name)
}

grid.arrange(
  gg_name_area("Mia"),
  gg_name_area("James"),
  gg_name_area("Jolie"),
  gg_name_area("Tyler"),
  gg_name_area("Maddie")
)



gg_name_area2 <- function(id4_name=NULL) {
  ggplot(babynames %>% filter(name==id4_name),
         aes(x=year, y=n, fill=sex)) +
    geom_area(position="fill") +
    scale_fill_manual(values=c("hotpink", "darkblue")) +
    ggtitle(id4_name) +
    xlab("") +
    ylab("") +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
}

grid.arrange(
  gg_name_area2("Frankie"),
  gg_name_area2("Marion"),
  gg_name_area2("Peyton"),
  gg_name_area2("Chris"),
  gg_name_area2("Taylor")
)




bn %>%
  filter(name=="Mia", year==2000)




frankie <- bn %>%
  filter(name=="Frankie")

frankie$higher <- ifelse(frankie$nF > frankie$nM, "nF", "nM")

frankie_res <- rle(frankie$higher)

length(frankie_res$lengths)
