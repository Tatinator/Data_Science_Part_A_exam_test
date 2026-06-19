# Movie lenght analysis

Runtime_Distribution_Plot <- function(movies){

    g <-
        movies %>%
        ggplot(aes(x = runtime)) +
        geom_histogram(bins = 30) +
        theme_bw() +
        labs(
            title = "How Long is a Netflix Movie?",
            subtitle = "Distribution of movie runtime in minutes",
            x = "Runtime in minutes",
            y = "Number of movies"
        )

    g

}

Runtime_Distribution_Plot(movies)


# Runtime by Country

Country_Runtime_Plot <- function(movies_country, TopN = 15){

    rating_country <- movies_country %>%
        filter(production_countries %in% top_countries$production_countries,
               !is.na(imdb_score))

    g <-
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

    g

}

Country_Runtime_Plot(movies_country)



Runtime_Country_Table <- function(movies_country, TopN = 15){

    top_countries <-
        movies_country %>%
        count(production_countries, sort = TRUE) %>%
        slice_max(n, n = TopN) %>%
        pull(production_countries)

    df <-
        movies_country %>%
        filter(
            production_countries %in% top_countries,
            !is.na(runtime)
        ) %>%
        group_by(production_countries) %>%
        summarise(
            movies = n(),
            average_runtime = round(mean(runtime, na.rm = TRUE), 1),
            median_runtime = round(median(runtime, na.rm = TRUE), 1),
            .groups = "drop"
        ) %>%
        arrange(desc(average_runtime))

    df

}

Runtime_Country_Table(movies_country)


# Correlation between runtime & IMDb score

Runtime_Rating_Plot <- function(movies){

    g <-
        movies %>%
        ggplot(aes(x = runtime, y = imdb_score)) +
        geom_point(alpha = 0.4) +
        geom_smooth(method = "lm", se = FALSE) +
        theme_bw() +
        labs(
            title = "Does Length Buy Love?",
            subtitle = "Relationship between movie runtime and IMDb score",
            x = "Runtime in minutes",
            y = "IMDb score"
        )

    g

}

Runtime_Rating_Plot(movies)



Runtime_Correlation <- function(movies){

    cor(movies$runtime, movies$imdb_score, use = "complete.obs")

}

Runtime_Correlation(movies)
