# Text analysis of movie descriptions

Description_Words <- function(movies){

    description_words <-
        movies %>%
        select(title, description) %>%
        filter(!is.na(description)) %>%
        unnest_tokens(word, description) %>%
        anti_join(stop_words, by = "word") %>%
        filter(!str_detect(word, "^[0-9]+$"))

    description_words

}


Word_Count_Table <- function(movies, TopN = 20){

    df <-
        Description_Words(movies) %>%
        count(word, sort = TRUE) %>%
        slice_max(n, n = TopN)

    df

}

Word_Count_Table(movies)

Description_Wordcloud <- function(movies, MaxWords = 100){

    word_counts <-
        Description_Words(movies) %>%
        count(word, sort = TRUE)

    wordcloud(
        words = word_counts$word,
        freq = word_counts$n,
        max.words = MaxWords,
        colors = brewer.pal(8, "Dark2")
    )

}

Description_Wordcloud(movies)

# Country-specific text analysis using TF-IDF

Country_TFIDF_Table <- function(movies_country,
                                Countries = c("US", "IN", "GB", "KR", "JP"),
                                TopN = 10){

    tfidf_country <-
        movies_country %>%
        filter(
            production_countries %in% Countries,
            !is.na(description)
        ) %>%
        select(production_countries, description) %>%
        unnest_tokens(word, description) %>%
        anti_join(stop_words, by = "word") %>%
        filter(!str_detect(word, "^[0-9]+$")) %>%
        count(production_countries, word, sort = TRUE) %>%
        bind_tf_idf(word, production_countries, n) %>%
        arrange(desc(tf_idf)) %>%
        group_by(production_countries) %>%
        slice_max(tf_idf, n = TopN) %>%
        ungroup()

    tfidf_country

}

Country_TFIDF_Table(movies_country)


Country_TFIDF_Plot <- function(movies_country,
                               Countries = c("US", "IN", "GB", "KR", "JP"),
                               TopN = 10){

    dfplot <-
        Country_TFIDF_Table(
            movies_country = movies_country,
            Countries = Countries,
            TopN = TopN
        )

    g <-
        dfplot %>%
        ggplot(aes(
            x = reorder_within(word, tf_idf, production_countries),
            y = tf_idf,
            fill = production_countries
        )) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ production_countries, scales = "free") +
        scale_x_reordered() +
        coord_flip() +
        theme_bw() +
        labs(
            title = "Distinctive Words in Movie Descriptions by Country",
            subtitle = "TF-IDF highlights words unusually associated with each country",
            x = "",
            y = "TF-IDF"
        ) +
        theme(
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()
        )

    g

}

Country_TFIDF_Plot(movies_country)
