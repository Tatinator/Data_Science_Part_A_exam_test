# Which countries produce most Netflix movies

Country_Count_Plot <- function(movies_country, TopN = 15){

    dfplot <-
        movies_country %>%
        count(production_countries, sort = TRUE) %>%
        slice_max(n, n = TopN)

    g <-
        dfplot %>%
        ggplot(aes(x = reorder(production_countries, n), y = n)) +
        geom_col() +
        coord_flip() +
        theme_bw() +
        labs(
            title = "Netflix's Global Movie Shelf",
            subtitle = paste0("Top ", TopN, " production countries represented on Netflix"),
            x = "",
            y = "Number of movies"
        )

    g

}

Country_Count_Plot(movies_country)



# IMDb ratings by country

top_countries <- movies_country %>%
    count(production_countries, sort = TRUE) %>%
    slice_max(n, n = 15)

Country_Rating_Plot <- function(movies_country, TopN = 15){

    rating_country <- movies_country %>%
        filter(production_countries %in% top_countries$production_countries,
               !is.na(imdb_score))

    g <-    ggplot(rating_country,
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


    g

}

Country_Rating_Plot(movies_country)



# Summary table

Country_Rating_Table <- function(movies_country, TopN = 15){

    top_countries <-
        movies_country %>%
        count(production_countries, sort = TRUE) %>%
        slice_max(n, n = TopN) %>%
        pull(production_countries)

    df <-
        movies_country %>%
        filter(
            production_countries %in% top_countries,
            !is.na(imdb_score)
        ) %>%
        group_by(production_countries) %>%
        summarise(
            movies = n(),
            average_rating = round(mean(imdb_score, na.rm = TRUE), 2),
            median_rating = round(median(imdb_score, na.rm = TRUE), 2),
            .groups = "drop"
        ) %>%
        arrange(desc(average_rating))

    df

}

Country_Rating_Table(movies_country)
