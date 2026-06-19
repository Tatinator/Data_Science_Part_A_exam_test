# Packages used

library(tidyverse)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggrepel)
library(scales)

# Read the data

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


# Create a Value Score
coffee <- coffee %>%
    mutate(
        ValueScore = Rating / Cost_Per_100g
    )

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
