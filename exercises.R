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


#-------------------------------------------------------------------------------
### DPLYR Exercises
# ---


# 1. How many penguins were included in the study?
#    ---------------------------------------------------------------------------




# 2. Which species were sampled? And on which Islands?
#    ---------------------------------------------------------------------------



# 3. Which species has the lowest proportion of females in the sample?
#    ---------------------------------------------------------------------------
# Tip: use p = n/sum(n) to calculate proportions




# 4. How long is the shortest bill? What species is that penguin, and how heavy
#    is that penguin?
#    ---------------------------------------------------------------------------



# 5. Does that penguin also have lowest weight? 
#    ---------------------------------------------------------------------------



# 6. Which penguin has the longest flipper? What is its species and how heavy
#    is it?
#    ---------------------------------------------------------------------------



# 7. Create a new variable `bill_ratio` which represents the ratio of the 
#    bill length to the bill depth. Which species has the hightest mean ratio?
#    ---------------------------------------------------------------------------



# 8. From now on, lets only work with a subset of penguins with complete data.
#    ---------------------------------------------------------------------------

penguin %>% 
  drop_na() 

# Be very careful with `drop_na`, it throws away all patients with any missing 
# variables. A safer approach is to identify what variables you want to exclude
# the observations with missing variables. 

penguin_c <- penguin %>% 
  filter(!is.na(bill_length_mm) & !is.na(bill_depth_mm) & 
           !is.na(flipper_length_mm) & !is.na(sex))


# 9. Save the complete case dataset in the "processed" folder. Save it as 
#    *.csv, *.rds and *.xlsx (tip: use the write.xlsx from the xlsx package)
#    ---------------------------------------------------------------------------





# ----------------------------------------------------------------------------
### JOINING EXCERCISES ###
# ------
  
# Now we will analyse all flights from NYC airport in 2013

# Load the datasets: 

airlines <- read_rds("data/airlines.rds") # Airlines that fly from NYC
airports <- read_rds("data/airports.rds") # Airports that are flown to from NYC
planes <- read_rds("data/planes.rds") # Planes used for those flights
flights <- read_rds("data/flights.rds") # All flights from 2013


# Explore the datasets
flights
airports
airlines
planes



# 1. What flight number has the longest delay?
#    ---------------------------------------------------------------------------



# 2. Join datasets together, so we have more info for the flights:
#    ---------------------------------------------------------------------------


# 3. Can you give us more information on the flight with the longest delay?
#    What was the airport? Which airline was it? How old was the plane?
#    Tipp: Use select to only show the variables you're interested in.
#    ---------------------------------------------------------------------------



## 4. Which airline was on average the fastest? Which one had was the slowest?
#    ---------------------------------------------------------------------------



## 5. Is there a relationship between age of plane and delay? Plot a random
#     sample of 2000 flights. (Tip: use set.seed(1) so results are reproducible)
#    ---------------------------------------------------------------------------




# --------------------------------------------------------------------------
### GGPLOT exercises
#-----


penguins <- read_rds("processed/penguin_complete.rds")


# 1. Plot the relationship between flipper length and body mass. 
#    ---------------------------------------------------------------------------


# 2. Is there a difference between male and female penguins?
#    ---------------------------------------------------------------------------


# 3. Plot the relationship between bill length and bill depth. Use geom_smooth
#    to get a trend line.
#    ---------------------------------------------------------------------------


# 4. Do the same, but use method = "lm" for a linear model. What does our 
#    model imply?
#    ---------------------------------------------------------------------------


# 5. Have you heard of the Simpson's Paradox? Look at the same model, but for
#    each species individually. Tip: You can use colors or facet_wrap.
#    ---------------------------------------------------------------------------



# 6. Try different geoms. F.exp. plot the distribution of body weights. Can you 
#    spot which one is the heaviest pinguin?
#    ---------------------------------------------------------------------------


#    ---------------------------------------------------------------------------
# Now we switch to the TB dataset.
#    ---------------------------------------------------------------------------

# Below replicated is the graph of the TB data I showed you.
who_dat <- read_rds("data/who_clean.rds")


who_dat %>% 
  filter(country %in% c("Germany", "United States of America", 
                        "Brazil", "Afghanistan")) %>% 
  group_by(country, year) %>% 
  summarise(n = sum(value)) %>% 
  ggplot(aes(x = year, y = n)) + 
  geom_line(aes(color = country), size = 1) + 
  geom_point(aes(color = country), shape = 21, fill = "white", size = 3) + 
  facet_wrap(~country, scales = "free_y") + 
  labs(x = "Year", 
       y = "N of pulmonary TB cases", 
       color = "Country", 
       caption = "Data Source: Global Tuberculosis Report") + 
  theme_minimal() + 
  theme(legend.position = "None")



# 7. Adapt the code above to explore the same data for Switzerland, Ukraine 
#    and any country you like.
#    ---------------------------------------------------------------------------



# 8. I used a trick which is quite misleading. Y-axes are not the same on each 
# facet. Adapt the code and see what happens if you keep the same y-axis. 
#    ---------------------------------------------------------------------------




# 9. What would be an alternative to the different y scales?
#    ---------------------------------------------------------------------------



# ------------------------------------------------------------------------------
  
#### Now let's use a new dataset and look at surgeries

surgery <- read_rds("data/surgery_data.rds")

# The dataset contains 32'001 surgeries. Variables include covariates at
# the time of sugery (age, gender, race, baseline_cancer etc.) and several
# outcome variables (mort30 = 30-day mortaliy, complication = any complication
# after the surgery).
# 
# It was used for a paper that investigated the impact the hypotheses that
# surgery complications are more common at the end of the week or 
# at the end of the day.
# doi: 10.1213/ANE.0b013e3182315a6d

# Let's explore the first hypothesis. For that, let's create a graph to see: 

surgery %>% 
  group_by(dow) %>% # dow = day of week
  count(mort30) %>% 
  mutate(p = n/sum(n) * 100) %>% # we create percents here
  filter(mort30 == "Yes") %>% 
  ggplot(aes(x = dow, y = p)) + 
  geom_col() +
  labs(x = "Weekday", 
       y = "Infections per interventions (%)", 
       title = "Proportion of infections, stratified by weekday") + 
  theme_minimal(base_family = "Segoe UI") + 
  theme(plot.title = element_text(hjust = 0.5))


# 10. Now try to do the same for hour of the day. What do you conclude?
#    ---------------------------------------------------------------------------




# 11. Bonus question: What are the unadjusted and adjusted Odds ratios for
# infection, for each hour increase? Adjust for all variables that contain 
# "baseline_" age, and BMI. Tip #1: use glm() together with family = binomial.
# Tip #2: use tidy() from the broom package to extract model coefficients etc.
#    ---------------------------------------------------------------------------



