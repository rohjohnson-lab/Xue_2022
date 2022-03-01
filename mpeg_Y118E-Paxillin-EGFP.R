library(tidyverse)
library(janitor)
library(dplyr)
library(viridis)

# read in new csv. 
groupable <- read.csv("/Users/sophiavarady/Desktop/mpeg_y118e.csv")


mpeg_y118e_data <- function(untidy_data) {
  untidy_data <- clean_names(untidy_data)
  # convert x and y to numeric values 
  untidy_data$x <- as.numeric(untidy_data$x)
  untidy_data$y <- as.numeric(untidy_data$y)
  # do the same for distance metric
  untidy_data$x1_x2 <- as.numeric(untidy_data$x1_x2)
  untidy_data$y1_y2 <- as.numeric(untidy_data$y1_y2)
  # find initial value so that we can normalize position
  x_init <- untidy_data$x[1]
  y_init <- untidy_data$y[1]
# clean up the data
  untidy_data %>% 
    # these are the only columns we're using
    select(x, y, x1_x2, y1_y2, fish) %>%
    # get rid of initial point since it doesn't fit the schema
    filter(!is.na(x1_x2)) %>%
    # swap absolute for relative position
    mutate(x = x - x_init,
           y = y - y_init) %>%
    # use distance metric to generate initial points
    mutate(
      x0 = (x1_x2 - x) * -1,
      y0 = (y1_y2 - y) * -1
    )
}



#make the fish column discrete
groupable$fish = as.character(groupable$fish)
# group by cell, then map accross it
per_cell <- groupable %>%
  group_by(cell) %>%
  group_modify(~ mpeg_y118e_data(.x)) %>% 
  ungroup()

# we want to normalize the direction of wound position 
# fish are grouped into subsets based on orientation of wound 
# we take x0 and y0 from previous function to set first points to create a line

subset_fish2 <- per_cell %>%
  filter(fish %in% c(2)) %>%
  mutate(x_new= x, y_new=y, x0_new=x0, y0_new=y0) %>%
  select(fish, cell, y_new, x_new, x0_new, y0_new)

subset_fish3 <- per_cell %>%
  filter(fish %in% c(3)) %>%
  mutate(x_new= y, y_new= abs(x), x0_new=y0, y0_new=abs(x0)) %>%
  select(fish, cell, y_new, x_new, x0_new, y0_new)


subset_fish1_6 <- per_cell %>%
  filter(fish %in% c(1,6)) %>%
  mutate(x_new=abs(x), y_new=y, x0_new=abs(x0), y0_new=y0) %>%
  select(x_new, y_new, fish, cell, x0_new, y0_new)


subset_fish5 <- per_cell %>%
  filter(fish %in% c(5)) %>%
  mutate(x_new=abs(x), y_new=y, x0_new=abs(x0), y0_new=y0) %>%
  select(x_new, y_new, fish, cell, x0_new, y0_new)


subset_fish4 <- per_cell %>%
  filter(fish %in% c(4)) %>%
  mutate(x_new=x, y_new=y, x0_new=x0, y0_new=y0) %>%
  select(x_new, y_new, fish, cell, x0_new, y0_new)

# we bind together the subsets that now include all the variables we need to make our plot
normalized_wound_data <- rbind(subset_fish2, subset_fish3, subset_fish4, subset_fish1_6, subset_fish5)


# we're making a plot of position, we want one line per cell, so we group by cell 
ggplot(normalized_wound_data, aes(x=x_new, y=y_new, group=cell)) +
  #draw a sequence of connected lines for the sequence of positions of each cell
  geom_segment(mapping=aes(x=x0_new, y=y0_new, xend=x_new, yend=y_new), color="#2C3E50") + 
  # set limits
  xlim(-15, 125) + ylim(-75, 75) +
  # draw quadrants
  geom_hline(aes(yintercept=0), lwd=1) +geom_vline(aes(xintercept=0), lwd=1) +
  # create title and axis labels
  labs(title="", x = "", y = "") +
  theme_bw() +
  theme(axis.ticks= element_blank(), axis.text = element_blank()) 
# theme(axis.line = element_line(colour = 'black', size = 2))
#  theme(axis.text = element_blank())
ggsave("/Users/sophiavarady/Desktop/mpegy118e_grey.tiff", width = 4, height=4, units="in")


