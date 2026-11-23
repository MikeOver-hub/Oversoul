setwd("/Users/mekaadegbola/Desktop/PORT")

library(dplyr)
library(car)
library(DoubleML)
library(mlr3)
library(mlr3learners)
library(data.table)
library(ggplot2)
library(splines)
library(glmnet)

data <- read.csv("customer_subscription_churn_usage_patterns.csv")

#Classifying & Cleaning
# Just 2024
data2 <- data %>% filter(!signup_date <= as.Date("2024-01-01"))
#Classifying into Quarters
data2 <- data2 %>%
  mutate(signup_quarter = paste0("Q", quarter(signup_date), " ", year(signup_date)))
#Assigning binary to churn
data2$churn_bin <- ifelse(data2$churn == "Yes", 1, 0)

#frequency checks
table(data2$churn_bin[data2$signup_quarter == "Q1 2024"])
table(data2$churn_bin[data2$signup_quarter == "Q2 2024"])
table(data2$churn_bin[data2$signup_quarter == "Q3 2024"])
table(data2$churn_bin[data2$signup_quarter == "Q4 2024"])

#Q1 and Q3 lead the chirn rate 

nmodel <- glm(churn_bin~monthly_fee, data=data2, family=binomial)
summary(nmodel)

# P value suggests monthly fee alone is not a determinant of a high churn rate, likely 
# co-influenced by other factors

model2 <- glm(churn_bin~monthly_fee + support_tickets + payment_failures + tenure_months, data=data2, family=binomial)
summary(model2)

# Extremely high stat sig on support tickets and payment failures suggest they are high 
# indicators of churn. 

cor(data2$support_tickets, data2$payment_failures)

# correlation value suggests that support tickets are not caused by payment failures

#DDML setup

formulaf = formula(" ~ -1 + bs(support_tickets + payment_failures + tenure_months)")
feautres_formula = data.frame((model.matrix(formulaf, data2)))

datamodel = data.table("churn_bin"= data2$churn_bin,
                       "monthly_fee" = data2$monthly_fee,
                       features_flex)

data_dml_flex_churn = DoubleMLData$new(datamodel, 
                                       y_col = "churn_bin",
                                       d_col = "monthly_fee",
                                       x_cols = c("support_tickets", "payment_failures", "tenure_months"))

data_dml_flex_churn

#LASSO

set.seed(456)
lasso = lrn("regr.cv_glmnet", alpha =1, nfolds = 10, s="lambda.min")
lasso_class = lrn("classif.cv_glmnet", alpha = 1, nfolds = 10, s="lambda.min")

dml_plr_lasso_churn = DoubleMLPLR$new(data_dml_flex_churn,
                                      ml_l = lasso,
                                      ml_m = lasso_class)

dml_plr_lasso_churn$fit()
dml_plr_lasso_churn$summary()



