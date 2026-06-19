library(tidyverse)
library(ggrepel)
library(ggplot2)

# Load data
Coffee <- function() {
    read.csv("data/Coffee/Coffee.csv")
}
coffee <- Coffee()

# Create one review column
coffee <- coffee %>%
    mutate(
        Review = paste(desc_1, desc_2, desc_3)
    )


# Create a Student Match Score
student_words <- c(
    "sweet","chocolate","cocoa",
    "caramel","honey",
    "fruit","berry","citrus",
    "floral","mouthfeel",
    "finish"
)

coffee <- coffee %>%
    mutate(
        Review = str_to_lower(Review),

        StudentMatch =
            str_count(
                Review,
                paste(student_words, collapse = "|")
            )
    )

# Top Student-Friendly Coffees
coffee %>%
    select(name, roaster, Rating, StudentMatch) %>%
    arrange(desc(StudentMatch)) %>%
    slice_max(StudentMatch, n = 20)

# Create a Value Score
coffee <- coffee %>%
    mutate(
        ValueScore = Rating / Cost_Per_100g
    )

# Best Value Coffees
coffee %>%
    select(name, roaster, Rating, Cost_Per_100g, ValueScore) %>%
    arrange(desc(ValueScore)) %>%
    slice_head(n = 20)

# Which Countries Perform Best
coffee %>%
    group_by(loc_country) %>%
    summarise(
        AverageRating = mean(Rating),
        AverageCost = mean(Cost_Per_100g),
        AverageMatch = mean(StudentMatch),
        NumberOfCoffees = n()
    ) %>%
    arrange(desc(AverageRating))

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


#Add column that unites the two origins
coffee <- coffee %>%
    unite(
        "Origins",
        origin_1,
        origin_2,
        sep = ", ",
        na.rm = TRUE,
        remove = FALSE
    )


#List for the top 15 origins
origin_summary <- coffee %>%
    group_by(Origins) %>%
    summarise(
        AverageRating = mean(Rating, na.rm = TRUE),
        AverageCost = mean(Cost_Per_100g, na.rm = TRUE),
        AverageMatch = mean(StudentMatch, na.rm = TRUE),
        n = n(),
        .groups = "drop"
    ) %>%
    filter(n >= 10)


#List for the top 15 origins
origin_summary <- origin_summary %>%
    mutate(
        MatchCategory = ntile(AverageMatch, 3),
        MatchCategory = factor(
            MatchCategory,
            levels = c(1, 2, 3),
            labels = c("Low", "Medium", "High")
        )
    )

#Plot for the top 15 origins
ggplot(
    plot_data,
    aes(
        x = AverageCost,
        y = AverageRating,
        size = NumberOfCoffees,
        colour = MatchCategory
    )
) +
    geom_point(alpha = 0.8) +
    geom_text_repel(
        aes(label = Origin),
        size = 3,
        max.overlaps = 10,
        show.legend = FALSE
    ) +
    scale_colour_manual(
        values = c(
            "Low" = "red",
            "Medium" = "orange",
            "High" = "darkgreen"
        )
    ) +
    labs(
        title = "Best Coffee Origins: Quality, Cost and Student Appeal",
        subtitle = "Top 15 origins by average rating; bubble size shows number of coffees",
        x = "Average Cost per 100g",
        y = "Average Rating",
        colour = "Student Match",
        size = "Number of Coffees"
    ) +
    theme_minimal()

# Which countries roast the top origins?
origins %>%
    filter(Origin %in% top_origins) %>%
    count(loc_country, Origin) %>%
    ggplot(
        aes(
            x = reorder(Origin, n),
            y = loc_country,
            fill = n
        )
    ) +
    geom_tile() +
    scale_fill_gradient(
        low = "lightblue",
        high = "darkblue"
    ) +
    labs(
        title = "Which Countries Roast the Top Origins?",
        x = "Origin",
        y = "Roaster Country",
        fill = "Count"
    ) +
    theme(
        axis.text.x = element_text(
            angle = 45,
            hjust = 1,
            size = 8
        )
    )


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

# Bubble graph like origin
ggplot(
    roaster_summary,
    aes(
        x = AverageCost,
        y = AverageRating,
        size = NumberOfCoffees,
        colour = MatchCategory,
        label = roaster
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
        xintercept = mean(roaster_summary$AverageCost),
        linetype = "dashed",
        colour = "grey50"
    ) +
    geom_hline(
        yintercept = mean(roaster_summary$AverageRating),
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
        title = "Top 20 Coffee Roasters",
        subtitle = "Quality, Cost and Student Appeal",
        x = "Average Cost per 100g (USD)",
        y = "Average Rating",
        colour = "Student Match",
        size = "Number of Coffees"
    ) +
    theme_minimal()



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
