# state deafult

State_Default_Table <- function(df,
                                MinLoans = 100){

    df %>%
        group_by(addr_state) %>%
        summarise(
            default_rate = mean(loan_status_binary, na.rm = TRUE),
            n = n(),
            .groups = "drop"
        ) %>%
        filter(n > MinLoans) %>%
        arrange(desc(default_rate))

}


State_Default_Plot <- function(df,
                               MinLoans = 100,
                               TopN = 15){

    dfplot <-
        State_Default_Table(df, MinLoans = MinLoans) %>%
        slice_max(default_rate, n = TopN)

    g <-
        dfplot %>%
        ggplot(aes(x = reorder(addr_state, default_rate), y = default_rate)) +
        geom_col() +
        coord_flip() +
        scale_y_continuous(labels = scales::percent) +
        theme_bw() +
        labs(
            title = "Do States Differ in Default Culture?",
            subtitle = paste0("Highest default states, minimum ", MinLoans, " loans"),
            x = "",
            y = "Default rate"
        )

    g

}


State_Logit <- function(df){

    model_state <-
        glm(
            loan_status_binary ~ addr_state + grade + dti + int_rate + term + annual_inc,
            data = df,
            family = binomial
        )

    model_state

}

# Texas default

Texas_Default_Table <- function(df){

    df %>%
        group_by(texas) %>%
        summarise(
            default_rate = mean(loan_status_binary, na.rm = TRUE),
            n = n(),
            .groups = "drop"
        ) %>%
        mutate(
            texas = ifelse(texas == 1, "Texas", "Other US states")
        )

}


Texas_Default_Plot <- function(df){

    dfplot <-
        Texas_Default_Table(df)

    g <-
        dfplot %>%
        ggplot(aes(x = texas, y = default_rate)) +
        geom_col() +
        scale_y_continuous(labels = scales::percent) +
        theme_bw() +
        labs(
            title = "Texas vs the Rest",
            subtitle = "Raw default rate comparison",
            x = "",
            y = "Default rate"
        )

    g

}


Texas_Logit <- function(df){

    model_texas <-
        glm(
            loan_status_binary ~ texas + grade + dti + int_rate + term + annual_inc,
            data = df,
            family = binomial
        )

    model_texas

}


Texas_Odds_Table <- function(df){

    model_texas <-
        Texas_Logit(df)

    broom::tidy(model_texas, exponentiate = TRUE, conf.int = TRUE)

}
