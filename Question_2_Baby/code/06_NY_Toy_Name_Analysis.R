# Good names for New York

baby_ny <- Baby_Names %>%
    filter(State == "NY") %>%
    group_by(Year, Gender, Name) %>%
    summarise(
        Count = sum(Count),
        .groups = "drop"
    )



Toy_Name_Scores_NY <- function(baby_ny){

    toy_name_scores_ny <-
        baby_ny %>%
        group_by(Name, Gender) %>%
        summarise(
            Total_Births = sum(Count),
            Years_Used = n_distinct(Year),
            Peak_Count = max(Count),
            Avg_Count = mean(Count),
            Volatility = sd(Count),
            .groups = "drop"
        ) %>%
        mutate(
            Volatility = replace_na(Volatility, 0),
            Stability = Years_Used / max(Years_Used),
            Popularity = Total_Births / max(Total_Births),
            Risk = Volatility / max(Volatility, na.rm = TRUE),
            Toy_Name_Score =
                (0.5 * Stability) +
                (0.4 * Popularity) -
                (0.1 * Risk)
        ) %>%
        arrange(desc(Toy_Name_Score))

    toy_name_scores_ny

}


Toy_Name_Score_Plot <- function(baby_ny,
                                TopN = 20){

    dfplot <-
        Toy_Name_Scores_NY(baby_ny) %>%
        slice_max(Toy_Name_Score, n = TopN)

    g <-
        dfplot %>%
        ggplot(aes(
            x = reorder(Name, Toy_Name_Score),
            y = Toy_Name_Score,
            fill = Gender
        )) +
        geom_col() +
        coord_flip() +
        theme_bw() +
        labs(
            title = "Best Toy Character Names for the New York Market",
            subtitle = "Names with high popularity, long usage and lower fad risk in NY",
            x = "",
            y = "Toy name score",
            fill = "Gender"
        )

    g

}


Toy_Name_Score_Plot(baby_ny)


Fad_Names_NY <- function(baby_ny,
                         MinBirths = 500){

    fad_names_ny <-
        baby_ny %>%
        group_by(Name, Gender) %>%
        summarise(
            Total_Births = sum(Count),
            Peak_Count = max(Count),
            Peak_Year = Year[which.max(Count)],
            Years_Used = n_distinct(Year),
            .groups = "drop"
        ) %>%
        mutate(
            Fad_Risk = Peak_Count / Total_Births
        ) %>%
        filter(Total_Births > MinBirths) %>%
        arrange(desc(Fad_Risk))

    fad_names_ny

}


Fad_Risk_Plot_NY <- function(baby_ny,
                             TopN = 20,
                             MinBirths = 500){

    dfplot <-
        Fad_Names_NY(baby_ny, MinBirths = MinBirths) %>%
        slice_max(Fad_Risk, n = TopN)

    g <-
        dfplot %>%
        ggplot(aes(
            x = reorder(Name, Fad_Risk),
            y = Fad_Risk,
            fill = Gender
        )) +
        geom_col() +
        coord_flip() +
        scale_y_continuous(labels = scales::percent) +
        theme_bw() +
        labs(
            title = "Names with High Fad Risk in New York",
            subtitle = "Large share of total births concentrated in one peak year",
            x = "",
            y = "Fad risk",
            fill = "Gender"
        )

    g

}

Fad_Risk_Plot_NY(baby_ny)
