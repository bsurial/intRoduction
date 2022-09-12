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
#    ---------------------------------------------------------------------------

penguin %>% 
  nrow()


# 2. Which species were sampled? And on which Islands?
#    ---------------------------------------------------------------------------

penguin %>% 
  count(species) # Adelie, Chinstrap, Gentoo

penguin %>% 
  count(island) # Biscoe, Dream, Torgersen


# 3. Which species has the lowest proportion of females in the sample?
#    ---------------------------------------------------------------------------

penguin %>% 
  group_by(species) %>% 
  count(sex) %>% 
  mutate(p = n / sum(n)) 

# In Gentoo we have the lowest proportion with 46.8% female 
# (48% in Adelie, 50% in Chinstrap). 
  


# 4. How long is the shortest bill? What species is that penguin, and how heavy
#    is that penguin?
#    ---------------------------------------------------------------------------

penguin %>% 
  arrange(bill_length_mm) # Adelie: 32.1mm, female penguin with 3050gram



# 5. Does that penguin also have lowest weight? 
#    ---------------------------------------------------------------------------


penguin %>% 
  arrange(body_mass_g) # No, the leightest penguin has 2700g (bill of 46.9 mm)



# 6. Which penguin has the longest flipper? What is its species and how heavy
#    is it?
#    ---------------------------------------------------------------------------

penguin %>% 
  arrange(desc(flipper_length_mm)) # Gentoo, 54.3, 5650g



# 7. Create a new variable `bill_ratio` which represents the ratio of the 
#    bill length to the bill depth. Which species has the hightest mean ratio?
#    ---------------------------------------------------------------------------

penguin %>% 
  mutate(bill_ratio = bill_length_mm / bill_depth_mm) %>% 
  group_by(species) %>% 
  summarise(mean_bill_ratio = mean(bill_ratio, na.rm = TRUE))

# Gentoo with 3.18, vs. Adelie 2.12 and Chinstrap 2.65


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
#    *.csv, *.rds and *.xlsx
#    ---------------------------------------------------------------------------


write_csv(penguin_c, "processed/penguin_complete.csv")
write_rds(penguin_c, "processed/penguin_complete.rds")
xlsx::write.xlsx(penguin_c, "processed/penguin_complete.xlsx")



# Optional excercise: ------------------------------------------------------

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



# Explore the same data for Switzerland, Ukraine and any country you like.

who_dat %>% 
  filter(country %in% c("Switzerland", "France", 
                        "China", "Ukraine")) %>% 
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


# I used a trick which is quite misleading. Y-axes are not the same on each 
# facet. Adapt the code and see what happens if you keep the same y-axis. 

who_dat %>% 
  filter(country %in% c("Switzerland", "France", 
                        "China", "Ukraine")) %>% 
  group_by(country, year) %>% 
  summarise(n = sum(value)) %>% 
  ggplot(aes(x = year, y = n)) + 
  geom_line(aes(color = country), size = 1) + 
  geom_point(aes(color = country), shape = 21, fill = "white", size = 3) + 
  facet_wrap(~country) + 
  labs(x = "Year", 
       y = "N of pulmonary TB cases", 
       color = "Country", 
       caption = "Data Source: Global Tuberculosis Report") + 
  theme_minimal() + 
  theme(legend.position = "None")


# What would be an alternative to the different y scales?

who_dat %>% 
  filter(country %in% c("Switzerland", "France", 
                        "China", "Ukraine")) %>% 
  group_by(country, year) %>% 
  summarise(n = sum(value)) %>% 
  ggplot(aes(x = year, y = n)) + 
  geom_line(aes(color = country), size = 1) + 
  geom_point(aes(color = country), shape = 21, fill = "white", size = 3) + 
  scale_y_log10() +
  facet_wrap(~country) + 
  labs(x = "Year", 
       y = "N of pulmonary TB cases", 
       color = "Country", 
       caption = "Data Source: Global Tuberculosis Report") + 
  theme_minimal() + 
  theme(legend.position = "None")





-------------------------------------------------------------------------------
  
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


# Now try to do the same for hour of the day. What do you conclude?


d %>% 
  group_by(hour) %>% 
  count(mort30) %>% 
  mutate(p = n/sum(n) * 100) %>% 
  ggplot(aes(x = hour, y = p)) + 
  geom_col(fill = "darkblue", alpha = 0.7) + 
  facet_wrap(~mort30, scales = "free_y") + 
  labs(x = "Time of day", 
       y = "Infections per interventions (%)", 
       title = "Proportion of infections, stratified by weekday") + 
  theme_minimal(base_family = "Segoe UI") + 
  theme(plot.title = element_text(hjust = 0.5))


# Bonus question: What are the unadjusted and adjusted Odds ratios for
# infection, for each hour increase? Adjust for all variables that contain 
# "baseline_" age, and BMI. Tip, use glm() together with family = binomial.


model_data <- surgery %>% 
  mutate(mort_cat = mort30 == "Yes") %>% 
  select(age, bmi, starts_with("baseline"), mort_cat, hour)


crude_m <- glm(mort_cat ~ hour, family = binomial, data = model_data)
m <- glm(mort_cat ~ hour + ., family = binomial, data = model_data)  

broom::tidy(crude_m, exp = TRUE, conf.int = TRUE) %>% 
  filter(term == "hour") # aOR = 1.13, 95% CI 1.07 to 1.19.

broom::tidy(m, exp = TRUE, conf.int = TRUE) %>% 
  filter(term == "hour") # aOR = 1.11, 95% CI 1.05 to 1.18.




