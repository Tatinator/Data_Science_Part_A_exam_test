library(tidyverse)
library(stringr)
Baby_Names <- read_rds("data/US_Baby_names/Baby_Names_By_US_State.rds")
Top_100_Billboard <- read_rds("data/US_Baby_names/charts.rds")
HBO_titles <- read_rds("data/US_Baby_names/HBO_titles.rds")
HBO_credits <- read_rds("data/US_Baby_names/HBO_credits.rds")

#Fix the HBO_credits data folder to be able to view it
HBO_credits <- HBO_credits %>%
    mutate(across(where(is.character), ~ iconv(.x, from = "", to = "UTF-8", sub = "")))

# First question: do popular names persist?

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

spikes <- baby_national %>%
    arrange(Name, Year) %>%
    group_by(Name, Gender) %>%
    mutate(
        Previous = lag(Count),
        Growth = Count - Previous,
        Growth_Ratio = Count / Previous
    ) %>%
    ungroup()

spikes_relative <- spikes %>%
    filter(!is.na(Growth_Ratio),
           Previous > 50) %>%
    arrange(desc(Growth_Ratio)) %>%
    select(Year, Name, Gender,
           Count, Previous,
           Growth_Ratio) %>%
    head(25)

spikes_absolute <- spikes %>%
    filter(!is.na(Growth_Ratio),
           Previous > 50) %>%
    arrange(desc(Growth)) %>%
    select(Year, Name, Gender,
           Count, Previous,
           Growth_Ratio) %>%
    head(25)

spikes_relative
spikes_absolute

spikes %>%
    arrange(desc(Growth)) %>%
    select(Year, Name, Count, Previous, Growth) %>%
    head(20)

spikes %>%
    filter(Name == "Whitney") %>%
    arrange(desc(Growth)) %>%
    select(Year, Gender, Name, Count, Previous, Growth, Growth_Ratio) %>%
    head(10)

baby_national %>%
    filter(Name == "Whitney") %>%
    ggplot(aes(Year, Count)) +
    geom_line()

spikes_ranked <- spikes %>%
    filter(!is.na(Growth), Previous > 50) %>%
    arrange(desc(Growth)) %>%
    mutate(Jump_Rank = row_number())

spikes_ranked %>%
    filter(Name == "Whitney") %>%
    select(Jump_Rank, Year, Gender, Name, Count, Previous, Growth)

baby_national %>%
    filter(Name == "Whitney") %>%
    ggplot(aes(Year, Count, colour = Gender)) +
    geom_line(linewidth = 1) +
    theme_minimal() +
    labs(title = "Whitney rose over several years, not as a one-year spike")

spikes <- baby_national %>%
    arrange(Name, Year) %>%
    group_by(Name, Gender) %>%
    mutate(
        Previous = lag(Count),
        Growth = Count - Previous,
        Growth_Ratio = Count / Previous
    ) %>%
    ungroup()



# 1. Before vs After 1990 (Test the hypothesis)

rank_summary <- rank_corr %>%
    mutate(
        Period = if_else(Year < 1990,
                         "Before 1990",
                         "1990 onwards")
    ) %>%
    group_by(Period, Gender, Lag) %>%
    summarise(
        Mean_Correlation = mean(Spearman, na.rm = TRUE),
        SD_Correlation = sd(Spearman, na.rm = TRUE),
        .groups = "drop"
    )

rank_summary


ggplot(rank_summary,
       aes(x = factor(Lag),
           y = Mean_Correlation,
           fill = Period)) +
    geom_col(position = "dodge") +
    facet_wrap(~Gender) +
    labs(
        title = "Name Persistence Before and After 1990",
        x = "Years Ahead",
        y = "Average Spearman Correlation"
    ) +
    theme_minimal()

# Interpretation: If: Before 1990 > After 1990 then the hypothesis is confirmed.
# -> Hypothesis confirmed

# Top 10 Name Spikes

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

top_spikes


# alternative with absolute growth:

top_spikes_absolute <- spikes %>%
    filter(
        !is.na(Growth),
        Previous > 50
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
    head(10)

top_spikes_absolute

ggplot(top_spikes,
       aes(x = reorder(Name, Growth_Ratio),
           y = Growth_Ratio,
           fill = Gender)) +
    geom_col() +
    coord_flip() +
    theme_minimal() +
    labs(
        title = "Largest Relative Name Spikes"
    )

ggplot(top_spikes_absolute,
       aes(x = reorder(Name, Growth),
           y = Growth,
           fill = Gender)) +
    geom_col() +
    coord_flip() +
    theme_minimal() +
    labs(
        title = "Largest Absolute Name Spikes"
    )

# Match Spikes to HBO Characters and Actors



hbo_names <- HBO_credits %>%
    mutate(
        HBO_Name = word(name, 1),
        HBO_Character = word(character, 1)
    )

actor_matches <- top_spikes %>%
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

character_matches <- top_spikes %>%
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

# Billboard Artist Matching

billboard_artists <- Top_100_Billboard %>%
    mutate(
        Artist_First =
            word(artist, 1)
    )

artist_matches <- top_spikes %>%
    inner_join(
        billboard_artists,
        by = c("Name" = "Artist_First")
    )

artist_matches


top_decade_names <- baby_national %>%
    mutate(Decade = floor(Year/10)*10) %>%
    group_by(Decade, Name) %>%
    summarise(Count = sum(Count), .groups = "drop") %>%
    group_by(Decade) %>%
    slice_max(Count, n = 10) %>%
    ungroup()

ggplot(top_decade_names,
       aes(x = reorder(Name, Count),
           y = Count,
           fill = factor(Decade))) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    facet_wrap(~Decade, scales = "free_y") +
    theme_minimal() +
    labs(
        title = "Top 10 Baby Names by Decade",
        x = "",
        y = "Total Births"
    )

heatmap_data <- baby_national %>%
    mutate(Decade = floor(Year/10)*10) %>%
    group_by(Decade, Name) %>%
    summarise(Count = sum(Count), .groups = "drop") %>%
    group_by(Decade) %>%
    slice_max(Count, n = 15)

ggplot(heatmap_data,
       aes(Name, factor(Decade), fill = Count)) +
    geom_tile() +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 90)
    ) +
    labs(
        title = "Most Popular Names by Decade"
    )


selected_names <- c(
    "Mary",
    "Jennifer",
    "Jessica",
    "Emma",
    "Whitney"
)

baby_national %>%
    filter(Name %in% selected_names) %>%
    ggplot(aes(Year, Count, colour = Name)) +
    geom_line(linewidth = 1) +
    theme_minimal() +
    labs(
        title = "Evolution of Selected Baby Names"
    )


# Good names for New York

baby_ny <- Baby_Names %>%
    filter(State == "NY") %>%
    group_by(Year, Gender, Name) %>%
    summarise(
        Count = sum(Count),
        .groups = "drop"
    )

toy_name_scores_ny <- baby_ny %>%
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
        Toy_Name_Score = (0.5 * Stability) +
            (0.4 * Popularity) -
            (0.1 * Risk)
    ) %>%
    arrange(desc(Toy_Name_Score))

toy_name_scores_ny %>%
    select(Name, Gender, Total_Births, Years_Used, Volatility, Toy_Name_Score) %>%
    head(20)

toy_name_scores_ny %>%
    slice_max(Toy_Name_Score, n = 20) %>%
    ggplot(aes(
        x = reorder(Name, Toy_Name_Score),
        y = Toy_Name_Score,
        fill = Gender
    )) +
    geom_col() +
    coord_flip() +
    theme_minimal() +
    labs(
        title = "Best Toy Character Names for the New York Market",
        subtitle = "Names with high popularity, long usage and lower fad risk in NY",
        x = "",
        y = "Toy name score"
    )

fad_names_ny <- baby_ny %>%
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
    arrange(desc(Fad_Risk))

fad_names_ny %>%
    filter(Total_Births > 500) %>%
    select(Name, Gender, Peak_Year, Total_Births, Peak_Count, Fad_Risk) %>%
    head(20)

