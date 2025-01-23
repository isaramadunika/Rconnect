library(tidyverse)
library(palmerpenguins)

penguins %>%
  ggplot(aes(x=bill_depth_mm))+
  geom_histogram()
# added the anothe chart

penguins %>%
  ggplot(aes(x=bill_length_mm))+
  geom_boxplot()
