# Packages used across the Netflix analysis

library(tidyverse)
library(tidytext)
library(stringr)
library(scales)
library(wordcloud)
library(RColorBrewer)

# Read the data

Titles <- read_rds("data/netflix/titles.rds")
Credits <- read_rds("data/netflix/credits.rds")
Movie_Info <- read.csv("data/netflix/netflix_movies.csv")

HBO_titles <- read_rds("C:/Users/Tatjana/Desktop/Stellenbosch/Uni/Kurse/Data Science/Data Part A/Exam/31074774/Question_2_Baby/data/US_Baby_names/HBO_titles.rds")
HBO_credits <- read_rds("C:/Users/Tatjana/Desktop/Stellenbosch/Uni/Kurse/Data Science/Data Part A/Exam/31074774/Question_2_Baby/data/US_Baby_names/HBO_credits.rds")



# Cleaning data


# Clean titles data

movies <-
    Titles %>%
    filter(type == "MOVIE") %>%
    filter(release_year <= 2022) %>%
    mutate(
        genres = str_remove_all(genres, "\\[|\\]|'"),
        production_countries = str_remove_all(production_countries, "\\[|\\]|'")
    )

# Create one row per country
movies_country <- movies %>%
    separate_rows(production_countries, sep = ", ") %>%
    filter(production_countries != "")

# Create one row per genre
movies_genre <- movies %>%
    separate_rows(genres, sep = ", ") %>%
    filter(genres != "")


# Clean HBO genres and countries

hbo_clean <- HBO_titles %>%
    mutate(
        genres = str_remove_all(genres, "\\[|\\]|'"),
        production_countries = str_remove_all(production_countries, "\\[|\\]|'")
    )

# Create one row per country

HBO_Country <- function(hbo_clean){

    hbo_countries <-
        hbo_clean %>%
        separate_rows(production_countries, sep = ", ") %>%
        filter(production_countries != "")

    hbo_countries

}

# Create one row per genre

HBO_Genre <- function(hbo_clean){

    hbo_genres <-
        hbo_clean %>%
        separate_rows(genres, sep = ", ") %>%
        filter(genres != "")

    hbo_genres

}


