# Which Roast Strength Performs Best
coffee %>%
    distinct(roast)

coffee %>%
    mutate(
        roast = factor(
            roast,
            levels = c(
                "",
                "Light",
                "Medium-Light",
                "Medium",
                "Medium-Dark",
                "Dark"
            )
        )
    ) %>%
    group_by(roast) %>%
    summarise(
        AverageRating = mean(Rating, na.rm = TRUE),
        AverageMatch = mean(StudentMatch, na.rm = TRUE),
        AverageCost = mean(Cost_Per_100g, na.rm = TRUE)
    ) %>%
    arrange(roast)




# Create list for roast types
roast_summary <- coffee %>%
    mutate(
        roast = factor(
            roast,
            levels = c(
                "",
                "Light",
                "Medium-Light",
                "Medium",
                "Medium-Dark",
                "Dark"
            )
        )
    ) %>%
    group_by(roast) %>%
    summarise(
        AverageRating = mean(Rating, na.rm = TRUE),
        AverageMatch = mean(StudentMatch, na.rm = TRUE),
        AverageCost = mean(Cost_Per_100g, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    mutate(
        MatchCategory = ntile(AverageMatch, 3),
        MatchCategory = factor(
            MatchCategory,
            levels = c(1, 2, 3),
            labels = c("Low", "Medium", "High")
        )
    )

# Plot the Roast type performance graph
ggplot(
    roast_summary,
    aes(
        x = roast,
        y = AverageRating,
        fill = MatchCategory
    )
) +
    geom_col() +
    geom_line(
        aes(
            y = AverageCost * (max(AverageRating) / max(AverageCost)),
            group = 1
        ),
        colour = "black",
        linewidth = 1
    ) +
    geom_point(
        aes(
            y = AverageCost * (max(AverageRating) / max(AverageCost))
        ),
        colour = "black",
        size = 3
    ) +
    scale_fill_manual(
        values = c(
            "Low" = "red",
            "Medium" = "orange",
            "High" = "darkgreen"
        )
    ) +
    scale_y_continuous(
        name = "Average Rating",
        sec.axis = sec_axis(
            ~ . / (max(roast_summary$AverageRating) /
                       max(roast_summary$AverageCost)),
            name = "Average Cost per 100g (USD)"
        )
    ) +
    labs(
        title = "Roast Type Performance",
        subtitle = "Bar height = Rating | Colour = Student Match | Line = Cost",
        x = "Roast Type",
        fill = "Student Match"
    ) +
    theme_minimal()
