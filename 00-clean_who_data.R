library(tidyverse)

who_clean <- who %>% 
  select(country, iso2:year, new_sp_m014, new_sp_m1524) %>% 
  pivot_longer(-(country:year), values_drop_na = TRUE) %>%
  separate(col = name, sep = "_", into = c("new", "diag", "gender")) %>% 
  separate(gender, sep = "(?<=[mf])(?=[0-9])", into = c("gender", "agegroup")) %>% 
  mutate(agegroup = case_when(
    agegroup == "014" ~ "0-14 yrs",
    agegroup == "1524" ~ "15-24 yrs")) %>% 
  mutate(diag = case_when(
    diag == "sp" ~ "sputum smear"
  )) %>% 
  select(-new)


write_rds(who_clean, "data/who_clean.rds")
