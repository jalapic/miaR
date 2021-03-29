library(tidyverse)
library(gridExtra)
library(ggplot2)


## https://cran.r-project.org/web/packages/babynames/babynames.pdf
## https://github.com/hadley/babynames/blob/master/data/babynames.rda

load("/Users/mia/Downloads/babynames.rda")


gg_name <- function(idname = NULL, color1 = NULL, color2 = NULL) {
  p <- ggplot(babynames %>% filter(name==idname),
              aes(x=year, y=n, color=sex)) +
    geom_line(color1="hotpink", color1="darkblue") +
    ggtitle(idname) +
    scale_color_manual(values=c(color1,color2))
  return(p)
}

grid.arrange(
  gg_name("Mia", color1 = "hotpink", color2 = "darkblue"),
  gg_name("James", color1 = "hotpink", color2 = "darkblue"),
  gg_name("Jolie", color1 = "hotpink", color2 = "darkblue"),
  gg_name("Tyler", color1 = "hotpink", color2 = "darkblue"),
  gg_name("Maddie", color1 = "hotpink", color2 = "darkblue")
)

df <- babynames %>% 
  filter(year == 2002) %>%
  group_by(name, sex) %>%
  summarize(total = sum(n)) %>%
  pivot_wider(
    names_from = sex, values_from = total, values_fill = 0)

colnames(df) <- c("name", "nM", "nF")

df <- df %>% mutate(ratio = nM/nF, total = nM + nF)

unisex_names <- df %>%
  filter(ratio > .33, ratio < .67, total > 250)

unisex_names

babynames %>%
  filter(year==2002, name=="Mia")

babynames %>%
  filter(year==2001, name=="Maria")













