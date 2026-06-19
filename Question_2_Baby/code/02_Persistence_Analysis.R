# Do popular names persist?

baby_national <- Baby_Names %>%
    group_by(Year, Gender, Name) %>%
    summarise(
        Count = sum(Count),
        .groups = "drop"
    )


top25 <- baby_national %>%
    group_by(Year, Gender) %>%
    arrange(desc(Count), .by_group = TRUE) %>%
    slice_head(n = 25) %>%
    mutate(Rank = row_number()) %>%
    ungroup()



# Spearman correlation

rank_corr <- purrr::map_dfr(1:3, function(k) {

    current <- top25 %>%
        select(Year, Gender, Name, Rank_Current = Rank)

    future <- top25 %>%
        select(
            Future_Year = Year,
            Gender,
            Name,
            Rank_Future = Rank
        )

    current %>%
        inner_join(future, by = c("Gender", "Name")) %>%
        filter(Future_Year == Year + k) %>%
        group_by(Year, Gender) %>%
        summarise(
            Spearman = cor(
                Rank_Current,
                Rank_Future,
                method = "spearman"
            ),
            Overlap = n(),
            Lag = k,
            .groups = "drop"
        )

})


rank_corr <- purrr::map_dfr(1:3, function(k) {

    top25 %>%
        select(Year, Gender, Name, Rank_Current = Rank) %>%
        inner_join(
            top25 %>%
                mutate(Year = Year - k) %>%
                select(Year, Gender, Name, Rank_Future = Rank),
            by = c("Year", "Gender", "Name")
        ) %>%
        group_by(Year, Gender) %>%
        summarise(
            Spearman = cor(Rank_Current, Rank_Future, method = "spearman"),
            Overlap = n(),
            Lag = k,
            .groups = "drop"
        )
})

ggplot(rank_corr,
       aes(x = Year,
           y = Spearman,
           colour = Gender)) +
    geom_line() +
    facet_wrap(~Lag) +
    theme_minimal() +
    labs(
        title = "Persistence of Popular Baby Names",
        subtitle = "Spearman rank correlation of top 25 names",
        y = "Correlation"
    )





# Suspicion of slower persistence before vs After 1990

Persistence_Before_After_Table <- function(rank_corr,
                                           BreakYear = 1990){

    rank_summary <-
        rank_corr %>%
        mutate(
            Period =
                if_else(Year < BreakYear,
                        paste0("Before ", BreakYear),
                        paste0(BreakYear, " onwards"))
        ) %>%
        group_by(Period, Gender, Lag) %>%
        summarise(
            Mean_Correlation = mean(Spearman, na.rm = TRUE),
            SD_Correlation = sd(Spearman, na.rm = TRUE),
            .groups = "drop"
        )

    rank_summary

}

Persistence_Before_After_Table(rank_corr)



Persistence_Before_After_Plot <- function(rank_corr,
                                          BreakYear = 1990){

    dfplot <-
        Persistence_Before_After_Table(rank_corr, BreakYear = BreakYear)

    g <-
        dfplot %>%
        ggplot(aes(
            x = factor(Lag),
            y = Mean_Correlation,
            fill = Period
        )) +
        geom_col(position = "dodge") +
        facet_wrap(~ Gender) +
        theme_bw() +
        labs(
            title = paste0("Name Persistence Before and After ", BreakYear),
            subtitle = "Average Spearman rank correlation by gender and years ahead",
            x = "Years ahead",
            y = "Average Spearman correlation",
            fill = ""
        )

    g

}

Persistence_Before_After_Plot(rank_corr)


# Interpretation: If: Before 1990 > After 1990 then the hypothesis is confirmed.
# -> Hypothesis confirmed