# Spike analysis

spikes <-
    baby_national %>%
    arrange(Name, Gender, Year) %>%
    group_by(Name, Gender) %>%
    mutate(
        Previous = lag(Count),
        Growth = Count - Previous,
        Growth_Ratio = Count / Previous
    ) %>%
    ungroup()


# Top relative spikes

Top_Relative_Spikes <- function(spikes,
                                TopN = 10,
                                MinPrevious = 50){

    spikes %>%
        filter(
            !is.na(Growth_Ratio),
            Previous > MinPrevious
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
        slice_head(n = TopN)

}

Top_Relative_Spikes(spikes)


Relative_Spike_Plot <- function(spikes,
                                TopN = 10,
                                MinPrevious = 50){

    dfplot <-
        Top_Relative_Spikes(
            spikes,
            TopN = TopN,
            MinPrevious = MinPrevious
        )

    g <-
        dfplot %>%
        ggplot(aes(
            x = reorder(Name, Growth_Ratio),
            y = Growth_Ratio,
            fill = Gender
        )) +
        geom_col() +
        coord_flip() +
        theme_bw() +
        labs(
            title = "Largest Relative Name Spikes",
            subtitle = paste0("Names with at least ", MinPrevious, " births in the previous year"),
            x = "",
            y = "Growth ratio",
            fill = "Gender"
        )

    g

}

Relative_Spike_Plot(spikes)


# Top absolute spikes

Top_Absolute_Spikes <- function(spikes,
                                TopN = 10,
                                MinPrevious = 50){

    spikes %>%
        filter(
            !is.na(Growth),
            Previous > MinPrevious
        ) %>%
        arrange(desc(Growth)) %>%
        select(
            Year,
            Name,
            Gender,
            Count,
            Previous,
            Growth
        ) %>%
        slice_head(n = TopN)

}

Top_Absolute_Spikes(spikes)


Absolute_Spike_Plot <- function(spikes,
                                TopN = 10,
                                MinPrevious = 50){

    dfplot <-
        Top_Absolute_Spikes(
            spikes,
            TopN = TopN,
            MinPrevious = MinPrevious
        )

    g <-
        dfplot %>%
        ggplot(aes(
            x = reorder(Name, Growth),
            y = Growth,
            fill = Gender
        )) +
        geom_col() +
        coord_flip() +
        theme_bw() +
        labs(
            title = "Largest Absolute Name Spikes",
            subtitle = paste0("Names with at least ", MinPrevious, " births in the previous year"),
            x = "",
            y = "Additional births relative to previous year",
            fill = "Gender"
        )

    g

}

Absolute_Spike_Plot(spikes)


Selected_Name_Plot <- function(baby_national,
                               SelectedNames = c("Mary", "Jennifer", "Jessica", "Emma", "Whitney")){

    g <-
        baby_national %>%
        filter(Name %in% SelectedNames) %>%
        ggplot(aes(Year, Count, colour = Name)) +
        geom_line(linewidth = 1) +
        theme_bw() +
        labs(
            title = "Evolution of Selected Baby Names",
            subtitle = "Long-run popularity patterns for chosen examples",
            x = "Year",
            y = "Birth count",
            colour = "Name"
        )

    g

}

Selected_Name_Plot(baby_national)


Name_Jump_Rank <- function(spikes,
                           SelectedName = "Whitney",
                           MinPrevious = 50){

    spikes %>%
        filter(!is.na(Growth), Previous > MinPrevious) %>%
        arrange(desc(Growth)) %>%
        mutate(Jump_Rank = row_number()) %>%
        filter(Name == SelectedName) %>%
        select(Jump_Rank, Year, Gender, Name, Count, Previous, Growth, Growth_Ratio)

}

Name_Jump_Rank(spikes)


baby_national %>%
    filter(Name == "Whitney") %>%
    ggplot(aes(Year, Count, colour = Gender)) +
    geom_line(linewidth = 1) +
    theme_minimal() +
    labs(title = "Whitney rose over several years, not as a one-year spike")

