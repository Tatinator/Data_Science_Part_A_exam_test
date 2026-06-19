# Director analysis using credits data

Director_Data <- function(Credits, movies){

    directors <-
        Credits %>%
        filter(role == "DIRECTOR") %>%
        inner_join(movies, by = "id")

    directors

}

Top_Directors_Plot <- function(Credits, movies, TopN = 10){

    dfplot <-
        Director_Data(Credits, movies) %>%
        count(name, sort = TRUE) %>%
        slice_max(n, n = TopN)

    g <-
        dfplot %>%
        ggplot(aes(x = reorder(name, n), y = n)) +
        geom_col() +
        coord_flip() +
        theme_bw() +
        labs(
            title = "Most Common Directors on Netflix",
            subtitle = "A simple view of catalogue depth by director",
            x = "",
            y = "Number of movies"
        )

    g

}

Top_Directors_Plot(Credits, movies)

Director_Rating_Table <- function(Credits, movies, MinMovies = 3){

    df <-
        Director_Data(Credits, movies) %>%
        filter(!is.na(imdb_score)) %>%
        group_by(name) %>%
        summarise(
            movies = n(),
            average_rating = round(mean(imdb_score, na.rm = TRUE), 2),
            .groups = "drop"
        ) %>%
        filter(movies >= MinMovies) %>%
        arrange(desc(average_rating))

    df

}

Director_Rating_Table(Credits, movies)
