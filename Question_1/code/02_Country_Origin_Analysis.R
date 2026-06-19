
# Which countries roast the top origins?
origins <- coffee %>%
    select(name, roaster, loc_country, Rating, StudentMatch, Cost_Per_100g, origin_1, origin_2) %>%
    pivot_longer(
        cols = c(origin_1, origin_2),
        names_to = "OriginType",
        values_to = "Origin"
    ) %>%
    filter(!is.na(Origin), Origin != "")

top_origins <- origins %>%
    group_by(Origin) %>%
    summarise(
        AverageRating = mean(Rating, na.rm = TRUE),
        n = n(),
        .groups = "drop"
    ) %>%
    filter(n >= 10) %>%
    slice_max(AverageRating, n = 15) %>%
    pull(Origin)




#List for the top 20 origin plot
origin_summary <- origins %>%
    group_by(Origin) %>%
    summarise(
        AverageRating = mean(Rating, na.rm = TRUE),
        AverageCost = mean(Cost_Per_100g, na.rm = TRUE),
        AverageMatch = mean(StudentMatch, na.rm = TRUE),
        NumberOfCoffees = n(),
        .groups = "drop"
    ) %>%
    filter(NumberOfCoffees >= 10) %>%
    arrange(
        desc(AverageRating),
        desc(AverageMatch)
    ) %>%
    slice_head(n = 20) %>%
    mutate(
        MatchCategory = ntile(AverageMatch, 3),
        MatchCategory = factor(
            MatchCategory,
            levels = c(1, 2, 3),
            labels = c("Low", "Medium", "High")
        )
    )

ggplot(
    origin_summary,
    aes(
        x = AverageCost,
        y = AverageRating,
        size = NumberOfCoffees,
        colour = MatchCategory,
        label = Origin
    )
) +
    geom_point(alpha = 0.8) +
    geom_text_repel(
        size = 3,
        box.padding = 0.4,
        point.padding = 0.3,
        max.overlaps = Inf,
        show.legend = FALSE
    ) +
    geom_vline(
        xintercept = mean(origin_summary$AverageCost),
        linetype = "dashed",
        colour = "grey50"
    ) +
    geom_hline(
        yintercept = mean(origin_summary$AverageRating),
        linetype = "dashed",
        colour = "grey50"
    ) +
    scale_colour_manual(
        values = c(
            "High" = "darkgreen",
            "Medium" = "orange",
            "Low" = "red"
        )
    ) +
    labs(
        title = "Top 20 Coffee Origins",
        subtitle = "Quality, Cost and Student Appeal",
        x = "Average Cost per 100g (USD)",
        y = "Average Rating",
        colour = "Student Match",
        size = "Number of Coffees"
    ) +
    theme_minimal()
