# Roaster Analysis


#List for the roasting plot
roaster_summary <- coffee %>%
    group_by(roaster) %>%
    summarise(
        AverageRating = mean(Rating, na.rm = TRUE),
        AverageMatch = mean(StudentMatch, na.rm = TRUE),
        AverageCost = mean(Cost_Per_100g, na.rm = TRUE),
        NumberOfCoffees = n(),
        .groups = "drop"
    ) %>%
    filter(NumberOfCoffees >= 5) %>%   # avoids tiny roasters
    arrange(
        desc(AverageRating),
        desc(AverageMatch)
    ) %>%
    slice_head(n = 20) %>%             # top 20 roasters
    mutate(
        MatchCategory = ntile(AverageMatch, 3),
        MatchCategory = factor(
            MatchCategory,
            levels = c(1, 2, 3),
            labels = c("Low", "Medium", "High")
        )
    )



# Bubble bar chart Roasters
roaster_summary %>%
    arrange(desc(AverageRating)) %>%
    ggplot(aes(
        x = AverageRating,
        y = reorder(roaster, AverageRating),
        colour = MatchCategory,
        size = NumberOfCoffees
    )) +
    geom_point() +
    scale_colour_manual(
        values = c(
            "High" = "darkgreen",
            "Medium" = "orange",
            "Low" = "red"
        )
    ) +
    labs(
        title = "Top 20 Roasters by Average Rating",
        x = "Average Rating",
        y = "",
        colour = "Student Match",
        size = "Number of Coffees"
    ) +
    theme_minimal()