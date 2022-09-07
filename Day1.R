library(tidyverse)

# Read data from internet
url <- "https://raw.githubusercontent.com/bsurial/intRoduction/main/data/penguin.csv"

penguin <- read_csv(url)
penguin_df <- read.csv(url)

# Look at the differences between a tibble and a standard data.frame

penguin_df # conventional dataframe
penguin # tibble dataframe


# From now on, lets work with tibbles. Use `as_tibble()` to convert a 
# standard data.frame to a tibble


penguin_df %>% 
  as_tibble()



# Let's explore the dataset:

library(skimr) # This package allows to describe the dataset in a compact way.

skim(penguin)


# Questions
# ---

# 1. How many penguins were included in the study?

penguin %>% 
  nrow()


# 2. Which species were sampled? And on which Islands?

penguin %>% 
  count(species) # Adelie, Chinstrap, Gentoo

penguin %>% 
  count(island) # Biscoe, Dream, Torgersen


# 3. Which species has the lowest proportion of females in the sample?

penguin %>% 
  group_by(species) %>% 
  count(sex) %>% 
  mutate(p = n / sum(n)) 

# In Gentoo we have the lowest proportion with 46.8% female 
# (48% in Adelie, 50% in Chinstrap). 
  


# 4. Create a new variable `bill_ratio` which represents the ratio of the 
#    bill length to the bill depth. Which species has the hightest mean ratio?

penguin %>% 
  mutate(bill_ratio = bill_length_mm / bill_depth_mm) %>% 
  group_by(species) %>% 
  summarise(mean_bill_ratio = mean(bill_ratio, na.rm = TRUE))

# Gentoo with 3.18, vs. Adelie 2.12 and Chinstrap 2.65


# 5. From now on, lets only work with a subset of penguins with complete data.

penguin %>% 
  drop_na() 

# Be very careful with `drop_na`, it throws away all patients with any missing 
# variables. A safer approach is to identify what variables you want to exclude
# the observations with missing variables.

penguin_c <- penguin %>% 
  filter(!is.na(bill_length_mm) & !is.na(bill_depth_mm) & 
           !is.na(flipper_length_mm) & !is.na(sex))


# 6. Save the complete case dataset in the "processed" folder. Save it as 
#    *.csv, *.rds and *.xlsx

write_csv(penguin_c, "processed/penguin_complete.csv")
write_rds(penguin_c, "processed/penguin_complete.rds")
openxlsx::write.xlsx(penguin_c, "processed/penguin_complete.xlsx")



