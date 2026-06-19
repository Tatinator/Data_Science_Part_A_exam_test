# Top Names during the decades

Top_Decade_Names <- function(baby_national,
                             TopN = 10){

    top_decade_names <-
        baby_national %>%
        mutate(Decade = floor(Year / 10) * 10) %>%
        group_by(Decade, Name) %>%
        summarise(Count = sum(Count), .groups = "drop") %>%
        group_by(Decade) %>%
        slice_max(Count, n = TopN) %>%
        ungroup()

    top_decade_names

}

Top_Decade_Names(baby_national)


Top_Decade_Names_Plot <- function(baby_national,
                                  TopN = 10){

    dfplot <-
        Top_Decade_Names(baby_national, TopN = TopN)

    g <-
        dfplot %>%
        ggplot(aes(
            x = reorder(Name, Count),
            y = Count,
            fill = factor(Decade)
        )) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        facet_wrap(~ Decade, scales = "free_y") +
        theme_bw() +
        labs(
            title = paste0("Top ", TopN, " Baby Names by Decade"),
            x = "",
            y = "Total births"
        )     +
    theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

    g

}

Top_Decade_Names_Plot(baby_national)



Decade_Heatmap_Data <- function(baby_national,
                                TopN = 15){

    heatmap_data <-
        baby_national %>%
        mutate(Decade = floor(Year / 10) * 10) %>%
        group_by(Decade, Name) %>%
        summarise(Count = sum(Count), .groups = "drop") %>%
        group_by(Decade) %>%
        slice_max(Count, n = TopN) %>%
        ungroup()

    heatmap_data

}


Decade_Heatmap_Plot <- function(baby_national,
                                TopN = 15){

    dfplot <-
        Decade_Heatmap_Data(baby_national, TopN = TopN)

    g <-
        dfplot %>%
        ggplot(aes(Name, factor(Decade), fill = Count)) +
        geom_tile() +
        theme_bw() +
        theme(
            axis.text.x = element_text(angle = 90, hjust = 1)
        ) +
        labs(
            title = "Most Popular Names by Decade",
            subtitle = "A compact view of long-run naming fashion",
            x = "Name",
            y = "Decade",
            fill = "Births"
        )

    g

}

Decade_Heatmap_Plot(baby_national)
