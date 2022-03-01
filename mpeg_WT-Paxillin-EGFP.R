library(tidyverse)
library(janitor)
library(dplyr)
library(viridis)

# read in csv.
groupable_wt <- read.csv("/Users/sophiavarady/Desktop/mpeg_y118wt.csv")


fixed_data <- function(wt_data) {
  wt_data <- clean_names(wt_data)
  # convert x and y to numeric values
  wt_data$x <- as.numeric(wt_data$x)
  wt_data$y <- as.numeric(wt_data$y)
  # do the same for distance metric
  wt_data$x1_x2 <- as.numeric(wt_data$x1_x2)
  wt_data$y1_y2 <- as.numeric(wt_data$y1_y2)
  # find initial value so that we can normalize position
  x_init <- wt_data$x[1]
  y_init <- wt_data$y[1]
 
  wt_data %>% 
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
groupable_wt$fish = as.character(groupable_wt$fish)
# group by cell, then map accross it
per_cell_wt <- groupable_wt %>%
  group_by(cell) %>%
  group_modify(~ fixed_data(.x)) %>% 
  ungroup()


# we want to normalize the direction of wound position since each fish's wound is oriented distinctly
# we are normalizing to the "right side" position
# fish 4 and 5 need nothing
# we take x0 and y0 from previous function to set first points to create a line. do this for all subsets. 
subset_fish4_5_wt <- per_cell_wt %>%
  filter(fish %in% c(4,5)) %>%
  mutate(x_new= x, y_new=y, x0_new=x0, y0_new=y0) %>%
  select(fish, cell, y_new, x_new, x0_new, y0_new)

# wound is oriented to the bottom so we switch x and y and take the absolute value of y
subset_fish1_wt <- per_cell_wt %>%
  filter(fish %in% c(1)) %>%
  mutate(x_new= abs(y), y_new= x, x0_new=abs(y0), y0_new=x0) %>%
  select(fish, cell, y_new, x_new, x0_new, y0_new)

# wound is oriented to the top so we switch x and y
subset_fish3_wt <- per_cell_wt %>%
  filter(fish %in% c(3)) %>%
  mutate(x_new=y, y_new=x, x0_new=y0, y0_new=x0) %>%
  select(x_new, y_new, fish, cell, x0_new, y0_new)


subset_fish2_wt <- per_cell_wt %>%
  filter(fish %in% c(2)) %>%
  mutate(x_new=abs(x), y_new=y, x0_new=abs(x0), y0_new=y0) %>%
  select(x_new, y_new, fish, cell, x0_new, y0_new)


# we bind together the subsets 
normalized_wound_data_wt <- rbind(subset_fish4_5_wt, subset_fish1_wt, subset_fish3_wt, subset_fish2_wt)



# we're making a plot of position, we want one line per cell, so we group by cell 
ggplot(normalized_wound_data_wt, aes(x=x_new, y=y_new, group=cell)) +
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
ggsave("/Users/sophiavarady/Desktop/mpegy118wt_grey.tiff", width = 4, height=4, units="in")

