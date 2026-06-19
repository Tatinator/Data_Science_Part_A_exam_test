# Theories about Home ownership & long-term employment

Short_Term_Profile_Table <- function(df){

    df %>%
        filter(short_term == 1) %>%
        group_by(homeowner, emp_10plus) %>%
        summarise(
            default_rate = mean(loan_status_binary, na.rm = TRUE),
            n = n(),
            .groups = "drop"
        ) %>%
        mutate(
            homeowner = ifelse(homeowner == 1, "Owner / Mortgage", "Other"),
            emp_10plus = ifelse(emp_10plus == 1, "10+ years employed", "Less than 10 years")
        )

}

Short_Term_Profile_Table(df)


Short_Term_Profile_Plot <- function(df){

    dfplot <-
        Short_Term_Profile_Table(df)

    g <-
        dfplot %>%
        ggplot(aes(x = emp_10plus, y = default_rate, fill = homeowner)) +
        geom_col(position = "dodge") +
        scale_y_continuous(labels = scales::percent) +
        theme_bw() +
        labs(
            title = "Is the Heuristic Useful?",
            subtitle = "Short-term loan default rates by home ownership and employment length",
            x = "",
            y = "Default rate",
            fill = "Home ownership"
        )

    g

}

Short_Term_Profile_Plot(df)

Short_Term_Logit <- function(df){

    model_short <-
        glm(
            loan_status_binary ~ homeowner + emp_10plus + grade + dti + annual_inc,
            data = df %>% filter(short_term == 1),
            family = binomial
        )

    model_short

}

Short_Term_Logit(df)

Short_Term_Odds_Table <- function(df){

    model_short <-
        Short_Term_Logit(df)

    broom::tidy(model_short, exponentiate = TRUE, conf.int = TRUE) %>%
        arrange(desc(estimate))

}

Short_Term_Odds_Table(df)
