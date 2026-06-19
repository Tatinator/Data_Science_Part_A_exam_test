# Compare Netflix and HBO by genre

Platform_Genre_Comparison_Table <- function(movies_genre, hbo_genres, MinTitles = 20){

    netflix_genre_compare <-
        movies_genre %>%
        filter(!is.na(imdb_score)) %>%
        group_by(genres) %>%
        summarise(
            titles = n(),
            average_imdb = mean(imdb_score, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        filter(titles >= MinTitles) %>%
        mutate(platform = "Netflix")

    hbo_genre_compare <-
        hbo_genres %>%
        filter(!is.na(imdb_score)) %>%
        group_by(genres) %>%
        summarise(
            titles = n(),
            average_imdb = mean(imdb_score, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        filter(titles >= MinTitles) %>%
        mutate(platform = "HBO")

    genre_compare <-
        bind_rows(netflix_genre_compare, hbo_genre_compare)

    common_genres <-
        genre_compare %>%
        count(genres) %>%
        filter(n == 2) %>%
        pull(genres)

    genre_compare_common <-
        genre_compare %>%
        filter(genres %in% common_genres)

    genre_compare_common

}

Platform_Genre_Comparison_Table(movies_genre, hbo_genres)


Platform_Genre_Comparison_Plot <- function(movies_genre, hbo_genres, MinTitles = 20){

    dfplot <-
        Platform_Genre_Comparison_Table(
            movies_genre = movies_genre,
            hbo_genres = hbo_genres,
            MinTitles = MinTitles
        )

    g <-
        dfplot %>%
        ggplot(aes(
            x = reorder(genres, average_imdb),
            y = average_imdb,
            fill = platform
        )) +
        geom_col(position = "dodge") +
        coord_flip() +
        theme_bw() +
        labs(
            title = "Average IMDb Ratings by Genre: Netflix vs HBO",
            subtitle = "Common genres only",
            x = "",
            y = "Average IMDb score",
            fill = "Platform"
        )

    g

}

Platform_Genre_Comparison_Plot(movies_genre, hbo_genres)


# Compare Netflix and HBO catalogue strategy

Platform_Type_Summary <- function(Titles, hbo_clean){

    netflix_type_summary <-
        Titles %>%
        group_by(type) %>%
        summarise(
            titles = n(),
            average_imdb = round(mean(imdb_score, na.rm = TRUE), 2),
            .groups = "drop"
        ) %>%
        mutate(platform = "Netflix")

    hbo_type_summary <-
        hbo_clean %>%
        group_by(type) %>%
        summarise(
            titles = n(),
            average_imdb = round(mean(imdb_score, na.rm = TRUE), 2),
            .groups = "drop"
        ) %>%
        mutate(platform = "HBO")

    platform_type_summary <-
        bind_rows(netflix_type_summary, hbo_type_summary)

    platform_type_summary

}

Platform_Type_Plot <- function(Titles, hbo_clean){

    dfplot <-
        Platform_Type_Summary(Titles, hbo_clean)

    g <-
        dfplot %>%
        ggplot(aes(x = type, y = average_imdb, fill = platform)) +
        geom_col(position = "dodge") +
        theme_bw() +
        labs(
            title = "Average IMDb Rating by Content Type: Netflix vs HBO",
            subtitle = "A compact comparison of catalogue strategy",
            x = "Content type",
            y = "Average IMDb score",
            fill = "Platform"
        )

    g

}

Platform_Type_Plot(Titles, hbo_clean)



Platform_Country_Comparison_Plot <- function(movies_country, hbo_countries, TopN = 10){

    netflix_country_summary <-
        movies_country %>%
        group_by(production_countries) %>%
        summarise(titles = n(), .groups = "drop") %>%
        mutate(platform = "Netflix")

    hbo_country_summary <-
        hbo_countries %>%
        group_by(production_countries) %>%
        summarise(titles = n(), .groups = "drop") %>%
        mutate(platform = "HBO")

    country_compare <-
        bind_rows(netflix_country_summary, hbo_country_summary)

    top_compare_countries <-
        country_compare %>%
        group_by(production_countries) %>%
        summarise(total_titles = sum(titles), .groups = "drop") %>%
        slice_max(total_titles, n = TopN) %>%
        pull(production_countries)

    g <-
        country_compare %>%
        filter(production_countries %in% top_compare_countries) %>%
        ggplot(aes(
            x = reorder(production_countries, titles),
            y = titles,
            fill = platform
        )) +
        geom_col(position = "dodge") +
        coord_flip() +
        theme_bw() +
        labs(
            title = "Country Representation: Netflix vs HBO",
            subtitle = "Top countries across the combined catalogues",
            x = "",
            y = "Number of titles",
            fill = "Platform"
        )

    g

}

Platform_Country_Comparison_Plot(movies_country, hbo_countries)
