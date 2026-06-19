# Packages used

library(tidyverse)
library(stringr)


# Read the data

Baby_Names <- read_rds("data/US_Baby_names/Baby_Names_By_US_State.rds")
Top_100_Billboard <- read_rds("data/US_Baby_names/charts.rds")
HBO_titles <- read_rds("data/US_Baby_names/HBO_titles.rds")
HBO_credits <- read_rds("data/US_Baby_names/HBO_credits.rds")


#Fix the HBO_credits data folder to be able to view it
HBO_credits <- HBO_credits %>%
    mutate(across(where(is.character), ~ iconv(.x, from = "", to = "UTF-8", sub = "")))

