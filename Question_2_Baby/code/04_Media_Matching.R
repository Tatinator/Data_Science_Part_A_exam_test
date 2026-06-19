# Match Spikes to HBO Characters and Actors

hbo_names <- HBO_credits %>%
    mutate(
        HBO_Name = word(name, 1),
        HBO_Character = word(character, 1)
    )

top_spikes <- spikes %>%
    filter(
        !is.na(Growth_Ratio),
        Previous > 50
    ) %>%
    arrange(desc(Growth_Ratio)) %>%
    select(
        Year,
        Name,
        Gender,
        Count,
        Previous,
        Growth,
        Growth_Ratio
    ) %>%
    head(10)


# Actor Matches

Actor_Matches <- function(top_spikes,
                          HBO_credits){


    actor_matches <-
        top_spikes %>%
        inner_join(
            hbo_names,
            by = c("Name" = "HBO_Name")
        ) %>%
        select(
            Name,
            Year,
            Gender,
            title = id,
            Actor = name,
            Character = character
        )

    actor_matches

}

Actor_Matches(top_spikes)


# Charactor Matches

Character_Matches <- function(top_spikes,
                              HBO_credits){


    character_matches <-
        top_spikes %>%
        inner_join(
            hbo_names,
            by = c("Name" = "HBO_Character")
        ) %>%
        select(
            Name,
            Year,
            Gender,
            Actor = name,
            Character = character
        )

    character_matches

}

Character_Matches(top_spikes)



# Billboard Artist Matching

Billboard_Artist_Matches <- function(top_spikes,
                                     Top_100_Billboard){

    billboard_artists <-
        Top_100_Billboard %>%
        mutate(
            Artist_First = word(artist, 1)
        )

    artist_matches <-
        top_spikes %>%
        inner_join(
            billboard_artists,
            by = c("Name" = "Artist_First")
        )

    artist_matches

}


Billboard_Artist_Matches(top_spikes, Top_100_Billboard)

