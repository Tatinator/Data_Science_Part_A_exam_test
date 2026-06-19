library(tidyverse)
library(tidytext)
library(stringr)
library(scales)
library(wordcloud)
library(RColorBrewer)


#Read the data
Titles <- read_rds("data/netflix/titles.rds")
Credits <- read_rds("data/netflix/credits.rds")
Movie_Info <- read.csv("data/netflix/netflix_movies.csv")


# Clean titles data
movies <- Titles %>%
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



# Which countries produce most Netflix movies

top_countries <- movies_country %>%
    count(production_countries, sort = TRUE) %>%
    slice_max(n, n = 15)

ggplot(top_countries,
       aes(x = reorder(production_countries, n), y = n)) +
    geom_col() +
    coord_flip() +
    labs(
        title = "Top 15 Countries by Number of Netflix Movies",
        x = "Country",
        y = "Number of Movies"
    ) +
    theme_minimal()


# IMDb ratings by country

rating_country <- movies_country %>%
    filter(production_countries %in% top_countries$production_countries,
           !is.na(imdb_score))

ggplot(rating_country,
       aes(x = reorder(production_countries, imdb_score, median),
           y = imdb_score)) +
    geom_boxplot() +
    coord_flip() +
    labs(
        title = "IMDb Ratings by Country",
        x = "Country",
        y = "IMDb Rating"
    ) +
    theme_minimal()

# Summary table
rating_country_table <- rating_country %>%
    group_by(production_countries) %>%
    summarise(
        movies = n(),
        average_rating = round(mean(imdb_score, na.rm = TRUE), 2),
        median_rating = round(median(imdb_score, na.rm = TRUE), 2)
    ) %>%
    arrange(desc(average_rating))

rating_country_table



# Movie length / runtime analysis

ggplot(movies, aes(x = runtime)) +
    geom_histogram(bins = 30) +
    labs(
        title = "Distribution of Netflix Movie Runtime",
        x = "Runtime in Minutes",
        y = "Number of Movies"
    ) +
    theme_minimal()

ggplot(rating_country,
       aes(x = reorder(production_countries, runtime, median),
           y = runtime)) +
    geom_boxplot() +
    coord_flip() +
    labs(
        title = "Movie Runtime by Country",
        x = "Country",
        y = "Runtime in Minutes"
    ) +
    theme_minimal()

runtime_country_table <- movies_country %>%
    filter(production_countries %in% top_countries$production_countries,
           !is.na(runtime)) %>%
    group_by(production_countries) %>%
    summarise(
        movies = n(),
        average_runtime = round(mean(runtime, na.rm = TRUE), 1),
        median_runtime = round(median(runtime, na.rm = TRUE), 1)
    ) %>%
    arrange(desc(average_runtime))

runtime_country_table

# Most common movie genres

top_genres <- movies_genre %>%
    count(genres, sort = TRUE) %>%
    slice_max(n, n = 15)

ggplot(top_genres,
       aes(x = reorder(genres, n), y = n)) +
    geom_col() +
    coord_flip() +
    labs(
        title = "Most Common Netflix Movie Genres",
        x = "Genre",
        y = "Number of Movies"
    ) +
    theme_minimal()

# Which genres have highest IMDb ratings?

genre_rating_table <- movies_genre %>%
    filter(!is.na(imdb_score)) %>%
    group_by(genres) %>%
    summarise(
        movies = n(),
        average_rating = round(mean(imdb_score, na.rm = TRUE), 2),
        median_rating = round(median(imdb_score, na.rm = TRUE), 2)
    ) %>%
    filter(movies >= 20) %>%
    arrange(desc(average_rating))

genre_rating_table

ggplot(genre_rating_table %>% slice_max(average_rating, n = 10),
       aes(x = reorder(genres, average_rating),
           y = average_rating)) +
    geom_col() +
    coord_flip() +
    labs(
        title = "Highest Rated Netflix Movie Genres",
        x = "Genre",
        y = "Average IMDb Rating"
    ) +
    theme_minimal()

# Relationship between runtime and rating

ggplot(movies,
       aes(x = runtime, y = imdb_score)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
        title = "Relationship Between Runtime and IMDb Rating",
        x = "Runtime in Minutes",
        y = "IMDb Rating"
    ) +
    theme_minimal()

cor(movies$runtime, movies$imdb_score, use = "complete.obs")

# Text analysis of movie descriptions

description_words <- movies %>%
    select(title, description) %>%
    filter(!is.na(description)) %>%
    unnest_tokens(word, description) %>%
    anti_join(stop_words, by = "word") %>%
    filter(!str_detect(word, "^[0-9]+$"))

word_counts <- description_words %>%
    count(word, sort = TRUE)

word_counts %>% head(20)

wordcloud(
    words = word_counts$word,
    freq = word_counts$n,
    max.words = 100,
    colors = brewer.pal(8, "Dark2")
)

# Country-specific text analysis using TF-IDF

country_words <- movies_country %>%
    filter(production_countries %in% c("US", "IN", "GB", "KR", "JP"),
           !is.na(description)) %>%
    select(production_countries, description) %>%
    unnest_tokens(word, description) %>%
    anti_join(stop_words, by = "word") %>%
    filter(!str_detect(word, "^[0-9]+$"))

tfidf_country <- country_words %>%
    count(production_countries, word, sort = TRUE) %>%
    bind_tf_idf(word, production_countries, n) %>%
    arrange(desc(tf_idf))

tfidf_country %>%
    group_by(production_countries) %>%
    slice_max(tf_idf, n = 10) %>%
    ungroup()



ggplot(tfidf_country %>%
           group_by(production_countries) %>%
           slice_max(tf_idf, n = 10) %>%
           ungroup(),
       aes(x = reorder_within(word, tf_idf, production_countries),
           y = tf_idf,
           fill = production_countries)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ production_countries, scales = "free") +
    scale_x_reordered() +
    coord_flip() +
    labs(
        title = "Distinctive Words in Movie Descriptions by Country",
        x = "Word",
        y = "TF-IDF"
    ) +
    theme_minimal() +
    theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
    )


# Director analysis using credits data

directors <- Credits %>%
    filter(role == "DIRECTOR") %>%
    inner_join(movies, by = "id")

top_directors <- directors %>%
    count(name, sort = TRUE) %>%
    slice_max(n, n = 10)

ggplot(top_directors,
       aes(x = reorder(name, n), y = n)) +
    geom_col() +
    coord_flip() +
    labs(
        title = "Most Common Directors on Netflix",
        x = "Director",
        y = "Number of Movies"
    ) +
    theme_minimal()

director_rating <- directors %>%
    filter(!is.na(imdb_score)) %>%
    group_by(name) %>%
    summarise(
        movies = n(),
        average_rating = round(mean(imdb_score, na.rm = TRUE), 2)
    ) %>%
    filter(movies >= 3) %>%
    arrange(desc(average_rating))

director_rating

# Final useful tables for report

top_countries
rating_country_table
runtime_country_table
top_genres
genre_rating_table
director_rating



# ============================================================
# HBO COMPARISON SECTION
# ============================================================


# Load HBO data
HBO_titles <- read_rds("C:/Users/Tatjana/Desktop/Stellenbosch/Uni/Kurse/Data Science/Data Part A/Exam/31074774/Question_2_Baby/data/US_Baby_names/HBO_titles.rds")
HBO_credits <- read_rds("C:/Users/Tatjana/Desktop/Stellenbosch/Uni/Kurse/Data Science/Data Part A/Exam/31074774/Question_2_Baby/data/US_Baby_names/HBO_credits.rds")


# Clean HBO genres and countries
hbo_clean <- HBO_titles %>%
    mutate(
        genres = str_remove_all(genres, "\\[|\\]|'"),
        production_countries = str_remove_all(production_countries, "\\[|\\]|'")
    )

hbo_genres <- hbo_clean %>%
    separate_rows(genres, sep = ", ") %>%
    filter(genres != "")

hbo_countries <- hbo_clean %>%
    separate_rows(production_countries, sep = ", ") %>%
    filter(production_countries != "")


# ============================================================
# 5. Compare Netflix and HBO by genre
# ============================================================

netflix_genre_compare <- movies_genre %>%
    filter(!is.na(imdb_score)) %>%
    group_by(genres) %>%
    summarise(
        titles = n(),
        average_imdb = mean(imdb_score, na.rm = TRUE)
    ) %>%
    filter(titles >= 20) %>%
    mutate(platform = "Netflix")

hbo_genre_compare <- hbo_genres %>%
    filter(!is.na(imdb_score)) %>%
    group_by(genres) %>%
    summarise(
        titles = n(),
        average_imdb = mean(imdb_score, na.rm = TRUE)
    ) %>%
    filter(titles >= 20) %>%
    mutate(platform = "HBO")

genre_compare <- bind_rows(netflix_genre_compare, hbo_genre_compare)

common_genres <- genre_compare %>%
    count(genres) %>%
    filter(n == 2) %>%
    pull(genres)

genre_compare_common <- genre_compare %>%
    filter(genres %in% common_genres)

ggplot(genre_compare_common,
       aes(x = reorder(genres, average_imdb),
           y = average_imdb,
           fill = platform)) +
    geom_col(position = "dodge") +
    coord_flip() +
    labs(
        title = "Average IMDb Ratings by Genre: Netflix vs HBO",
        x = "Genre",
        y = "Average IMDb Score",
        fill = "Platform"
    ) +
    theme_minimal()

# ============================================================
# 6. Compare Netflix and HBO catalogue strategy
# ============================================================

netflix_type_summary <- Titles %>%
    group_by(type) %>%
    summarise(
        titles = n(),
        average_imdb = round(mean(imdb_score, na.rm = TRUE), 2)
    ) %>%
    mutate(platform = "Netflix")

hbo_type_summary <- hbo_clean %>%
    group_by(type) %>%
    summarise(
        titles = n(),
        average_imdb = round(mean(imdb_score, na.rm = TRUE), 2)
    ) %>%
    mutate(platform = "HBO")

platform_type_summary <- bind_rows(
    netflix_type_summary,
    hbo_type_summary
)

platform_type_summary

ggplot(platform_type_summary,
       aes(x = type, y = average_imdb, fill = platform)) +
    geom_col(position = "dodge") +
    labs(
        title = "Average IMDb Rating by Content Type: Netflix vs HBO",
        x = "Content Type",
        y = "Average IMDb Score",
        fill = "Platform"
    ) +
    theme_minimal()

# ============================================================
# 7. Country comparison: HBO vs Netflix
# ============================================================

netflix_country_summary <- movies_country %>%
    group_by(production_countries) %>%
    summarise(titles = n()) %>%
    mutate(platform = "Netflix")

hbo_country_summary <- hbo_countries %>%
    group_by(production_countries) %>%
    summarise(titles = n()) %>%
    mutate(platform = "HBO")

country_compare <- bind_rows(
    netflix_country_summary,
    hbo_country_summary
)

top_compare_countries <- country_compare %>%
    group_by(production_countries) %>%
    summarise(total_titles = sum(titles)) %>%
    slice_max(total_titles, n = 10) %>%
    pull(production_countries)

ggplot(country_compare %>%
           filter(production_countries %in% top_compare_countries),
       aes(x = reorder(production_countries, titles),
           y = titles,
           fill = platform)) +
    geom_col(position = "dodge") +
    coord_flip() +
    labs(
        title = "Country Representation: Netflix vs HBO",
        x = "Country",
        y = "Number of Titles",
        fill = "Platform"
    ) +
    theme_minimal()
