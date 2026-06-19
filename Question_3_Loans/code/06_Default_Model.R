# Identify predictors of loan default by use of the logistic regression model

Default_Logit <- function(df){

    model_default <-
        glm(
            loan_status_binary ~ grade + term + int_rate + dti +
                annual_inc + home_ownership + emp_length + purpose +
                addr_state + delinq_2yrs + inq_last_6mths + revol_util,
            data = df,
            family = binomial
        )

    model_default

}

Default_Logit(df)


# Which borrower characteristics increase or decrease default risk (with odds-ratio table)

Default_Odds_Table <- function(df){

    model_default <-
        Default_Logit(df)

    broom::tidy(
        model_default,
        exponentiate = TRUE,
        conf.int = TRUE
    ) %>%
        arrange(desc(estimate))

}

Default_Odds_Table(df)


# Calculates the AUC statistic to evaluate how accurately the model predicts loan defaults.

Default_Model_AUC <- function(df){

    model_default <-
        Default_Logit(df)

    df_pred <-
        df %>%
        mutate(
            predicted_default =
                predict(model_default, type = "response")
        )

    auc_value <-
        pROC::auc(
            response = df_pred$loan_status_binary,
            predictor = df_pred$predicted_default
        )

    auc_value

}

Default_Model_AUC(df)


# Creates an ROC curve to visualize the model's ability to distinguish defaulters from non-defaulters.

Default_ROC_Plot <- function(df){

    model_default <-
        Default_Logit(df)

    df_pred <-
        df %>%
        mutate(
            predicted_default =
                predict(model_default, type = "response")
        )

    roc_obj <-
        pROC::roc(
            response = df_pred$loan_status_binary,
            predictor = df_pred$predicted_default
        )

    plot(roc_obj,
         main = "Default Model ROC Curve")

}

Default_ROC_Plot(df)