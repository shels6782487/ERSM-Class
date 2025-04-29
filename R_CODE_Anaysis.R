###### Analysis Coding #####
# Ensure 'Year' is treated as a categorical variable and Class_Label###
PandL$Year <- as.factor(PandL$Year)
PandL$Class_Label <- as.factor((PandL$Class_Label))
# Fit the multiple linear regression model without interaction terms
model <- lm(Net.Income ~ Year  + Class_Label + Payroll.Expenses + Paper.Goods + Cleaning.Supplies, data = PandL)

# View the summary of the model
summary(model)

#Check Residuals
plot(model)

# Model with log transformation of Net Income to correct issues
model2 <- lm(log(Net.Income) ~ Year + Class_Label + Payroll.Expenses + 
               Paper.Goods + Cleaning.Supplies, data = PandL)

plot(model2)

### decided to try Robust Regression ###
library(MASS)

robust_model <- rlm(log(Net.Income) ~ Year + Class + Payroll.Expenses + 
                      Paper.Goods + Cleaning.Supplies, data = PandL_filtered)

summary(robust_model)

#### Compared Models #####

summary(model2)           # Regular linear model
summary(robust_model)     # Robust regression

#  compare residuals:
plot(robust_model$residuals)

##### Prediction Graph of NetIncome(log) with Regression Line###

ggplot(PandL_filtered, aes(x = Payroll.Expenses, y = actual_log_income)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_line(aes(y = fitted_robust), color = "darkred", size = 1.2) +
  labs(
    title = "Robust Regression: Fitted Line on Log(Net Income)",
    x = "Payroll Expenses",
    y = "Log(Net Income)"
  ) +
  theme_minimal()

#### Effects of Predictors on Log net income###
ggplot(coef_df, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point(size = 3, color = "darkblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "darkblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(
    title = "Effect of Predictors on Log(Net Income) (Robust Regression)",
    x = "Predictors",
    y = "Estimate (± 95% CI)"
  ) +
  theme_minimal(base_family = "sans", base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#f2f2f2", color = NA),
    panel.background = element_rect(fill = "#f2f2f2", color = NA),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
    text = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5)  # <- reduced size
  )
