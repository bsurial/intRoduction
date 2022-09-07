library(tidyverse)

url <- "https://raw.githubusercontent.com/bsurial/intRoduction/main/data/penguin.csv"

penguin_tbl <- read_csv(url)
penguin_df <- read.csv(url)

# Look at the differences between a tibble and a standard data.frame

penguin_df

penguin_tbl


# From now on, lets work with tibbles. Use `as_tibble()` to convert a 
# standard data.frame to a tibble


penguin_df %>% 
  as_tibble()



# Let's explore the dataset:

library(skimr) # This package allows to describe the dataset in a compact way.

skim(penguin_tbl)




