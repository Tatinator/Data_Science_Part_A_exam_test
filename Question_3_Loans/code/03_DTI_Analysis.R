# DTI defaults

DTI_Default_Table <- function(df,
                              Breaks = seq(0, 60, 5)){

    df %>%
        mutate(
            dti_band = cut(dti, breaks = Breaks)
        ) %>%
        group_by(dti_band) %>%
        summarise(
            default_rate = mean(loan_status_binary, na.rm = TRUE),
            n = n(),
            .groups = "drop"
        ) %>%
        filter(!is.na(dti_band))

}


# Hard-cap for DTIs

DTI_Default_Table(df)

DTI_Default_Plot <- function(df,
                             Breaks = seq(0, 60, 5)){

    dfplot <-
        DTI_Default_Table(df, Breaks = Breaks)

    g <-
        dfplot %>%
        ggplot(aes(x = dti_band, y = default_rate)) +
        geom_col() +
        scale_y_continuous(labels = scales::percent) +
        theme_bw() +
        labs(
            title = "Where Should the DTI Warning Light Turn Red?",
            subtitle = "Default rate by debt-to-income band",
            x = "DTI band",
            y = "Default rate"
        ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

    g

}

DTI_Default_Plot(df)


# Recommendation for the Caps

DTI_Cap_Recommendation <- function(df,
                                   Breaks = seq(0, 60, 5)){

    dti_table <-
        DTI_Default_Table(df, Breaks = Breaks)

    recommendation <-
        tibble(
            tolerance = c("Low default tolerance", "Medium default tolerance", "High default tolerance"),
            suggested_cap = c("20-25", "30", "35-40"),
            interpretation = c(
                "Conservative lenders should limit exposure before default risk rises too sharply.",
                "Balanced lenders may allow moderate DTI levels while monitoring grade and interest rate.",
                "Risk-seeking lenders may tolerate higher DTI, but should price the risk carefully."
            )
        )

    list(
        dti_table = dti_table,
        recommendation = recommendation
    )

}

DTI_Cap_Recommendation(df)
