# Most common movie genres

Genre_Count_Plot <- function(movies_genre, TopN = 15){

    dfplot <-
        movies_genre %>%
        count(genres, sort = TRUE) %>%
        slice_max(n, n = TopN)

    g <-
        dfplot %>%
        ggplot(aes(x = reorder(genres, n), y = n)) +
        geom_col() +
        coord_flip() +
        theme_bw() +
        labs(
            title = "Most Common Netflix Movie Genres",
            subtitle = "Catalogue breadth by genre",
            x = "",
            y = "Number of movies"
        )

    g

}

Genre_Count_Plot(movies_genre)

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


Genre_Rating_Table <- function(movies_genre, MinMovies = 20){

    df <-
        movies_genre %>%
        filter(!is.na(imdb_score)) %>%
        group_by(genres) %>%
        summarise(
            movies = n(),
            average_rating = round(mean(imdb_score, na.rm = TRUE), 2),
            median_rating = round(median(imdb_score, na.rm = TRUE), 2),
            .groups = "drop"
        ) %>%
        filter(movies >= MinMovies) %>%
        arrange(desc(average_rating))

    df

}

Genre_Rating_Table(movies_genre)


Genre_Rating_Plot <- function(movies_genre, TopN = 10, MinMovies = 20){

    dfplot <-
        Genre_Rating_Table(movies_genre, MinMovies = MinMovies) %>%
        slice_max(average_rating, n = TopN)

    g <-
        dfplot %>%
        ggplot(aes(x = reorder(genres, average_rating), y = average_rating)) +
        geom_col() +
        coord_flip() +
        theme_bw() +
        labs(
            title = "Highest Rated Netflix Movie Genres",
            subtitle = paste0("Only genres with at least ", MinMovies, " movies included"),
            x = "",
            y = "Average IMDb score"
        )

    g

}

Genre_Rating_Plot(movies_genre)
