library(tidyverse)
library(janitor)
library(dplyr)
library(viridis)

# read in new csv. 
groupable_f <- read.csv("/Users/sophiavarady/Desktop/mpeg_y118f.csv")

fixed_data_f <- function(f_data) {
  # fix the names 
  f_data <- clean_names(f_data)
  # convert x and y to numeric values 
  f_data$x <- as.numeric(f_data$x)
  f_data$y <- as.numeric(f_data$y)
  # do the same for distance metric
  f_data$x1_x2 <- as.numeric(f_data$x1_x2)
  f_data$y1_y2 <- as.numeric(f_data$y1_y2)
  # find initial value so that we can normalize position
  x_init <- f_data$x[1]
  y_init <- f_data$y[1]
  f_data %>% 
    # these are the only columns we're using
    select(x, y, x1_x2, y1_y2, fish, movie_date) %>%
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
groupable_f$fish = as.character(groupable_f$fish)
# group by cell, then map accross it
per_cell_f <- groupable_f %>%
  group_by(cell) %>%
  group_modify(~ fixed_data_f(.x)) %>% 
  ungroup()


# we normalize wound positions to more clearly represent cell trajectories

subset_fish4_100121 <- per_cell_f %>%
  filter(fish %in% c(4) & movie_date %in% c(100121)) %>%
  mutate(x_new= abs(x), y_new=y, x0_new=abs(x0), y0_new=y0) %>%
  select(fish, cell, y_new, x_new, x0_new, y0_new, movie_date)

subset_fish3_100121 <- per_cell_f %>%
  filter(fish %in% c(3) & movie_date %in% c(100121)) %>%
  mutate(x_new= abs(y), y_new= x, x0_new=abs(y0), y0_new=x0) %>%
  select(fish, cell, y_new, x_new, x0_new, y0_new, movie_date)

subset_fish2_100121 <- per_cell_f %>%
  filter(fish %in% c(2) & movie_date %in% c(100121)) %>%
  mutate(x_new=x, y_new=y, x0_new=x0, y0_new=y0) %>%
  select(x_new, y_new, fish, cell, x0_new, y0_new, movie_date)

subset_fish1_3_93021 <- per_cell_f %>%
  filter(fish %in% c(1,3) & movie_date %in% c(93021)) %>%
  mutate(x_new=abs(y), y_new=x, x0_new=abs(y0), y0_new=x0) %>%
  select(x_new, y_new, fish, cell, x0_new, y0_new, movie_date)

subset_fish4_3_62821 <- per_cell_f %>%
  filter(fish %in% c(4,3) & movie_date %in% c(62821)) %>%
  mutate(x_new=abs(x), y_new=y, x0_new=abs(x0), y0_new=y0) %>%
  select(x_new, y_new, fish, cell, x0_new, y0_new, movie_date)

subset_fish2_62821 <- per_cell_f %>%
  filter(fish %in% c(2) & movie_date %in% c(62821)) %>%
  mutate(x_new=x, y_new=y, x0_new=x0, y0_new=y0) %>%
  select(x_new, y_new, fish, cell, x0_new, y0_new, movie_date)

subset_fish1_62821 <- per_cell_f %>%
  filter(fish %in% c(1) & movie_date %in% c(62821)) %>%
  mutate(x_new=abs(y), y_new=x, x0_new=abs(y0), y0_new=x0) %>%
  select(x_new, y_new, fish, cell, x0_new, y0_new, movie_date)

# we bind together the subsets 
normalized_wound_data_f <- rbind(subset_fish4_100121, subset_fish3_100121, subset_fish2_100121, subset_fish1_3_93021, subset_fish2_62821, subset_fish4_3_62821, subset_fish1_62821)

# we're making a plot of position, we want one line per cell, so we group by cell 
ggplot(normalized_wound_data_f, aes(x=x_new, y=y_new, group=cell)) +
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
ggsave("/Users/sophiavarady/Desktop/mpegy118f_grey.tiff", width = 4, height=4, units="in")


