# Default by grade & subgrade
Default_By_Grade_Table <- function(df){

    df %>%
        group_by(grade) %>%
        summarise(
            default_rate = mean(loan_status_binary, na.rm = TRUE),
            n = n(),
            .groups = "drop"
        ) %>%
        arrange(grade)

}

Default_By_Grade_Table(df)


Grade_Default_Plot <- function(df){

    dfplot <-
        Default_By_Grade_Table(df)

    g <-
        dfplot %>%
        ggplot(aes(x = grade, y = default_rate)) +
        geom_col() +
        scale_y_continuous(labels = scales::percent) +
        theme_bw() +
        labs(
            title = "Do Lending Club Grades Sort Risk?",
            subtitle = "Default rate by credit grade",
            x = "Credit grade",
            y = "Default rate"
        )

    g

}

Grade_Default_Plot(df)


Default_By_Subgrade_Table <- function(df){

    df %>%
        group_by(sub_grade) %>%
        summarise(
            default_rate = mean(loan_status_binary, na.rm = TRUE),
            n = n(),
            .groups = "drop"
        ) %>%
        arrange(sub_grade)

}

Default_By_Subgrade_Table(df)


Subgrade_Default_Plot <- function(df){

    dfplot <-
        Default_By_Subgrade_Table(df)

    g <-
        dfplot %>%
        ggplot(aes(x = sub_grade, y = default_rate)) +
        geom_col() +
        scale_y_continuous(labels = scales::percent) +
        theme_bw() +
        labs(
            title = "Risk Gradients Within Credit Grades",
            subtitle = "Default rate by sub-grade",
            x = "Credit sub-grade",
            y = "Default rate"
        ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

    g

}

Subgrade_Default_Plot(df)


# Default by term

Default_By_Term_Table <- function(df){

    df %>%
        group_by(term) %>%
        summarise(
            default_rate = mean(loan_status_binary, na.rm = TRUE),
            n = n(),
            .groups = "drop"
        ) %>%
        arrange(term)

}

Default_By_Term_Table(df)


