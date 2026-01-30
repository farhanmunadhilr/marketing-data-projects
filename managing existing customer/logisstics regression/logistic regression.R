#######################
# Logistic Regression #
####################### 

## Install Packages (if needed)

## Load Packages and Set Seed
set.seed(1)

## Read in Logistic Regression data
logit <- read.csv(file.choose()) ## Choose retail_logit.csv file

## Transform and Create Data
logit$number_of_orders2 <- logit$number_of_orders^2
logit$lnrevenue <- log(logit$revenue + 1)

## Run Logistic Regression using GLM
logit_result <- glm(formula = purchase ~ lnrevenue + number_of_orders + number_of_orders2 + recency_days + 
	loyalty_card + married + income, data = logit, family = "binomial")
summary(logit_result)

## Pseudo R-square - McFadden
null_result <- glm(formula = purchase ~ 1, data = logit, family = "binomial")
1 - logLik(logit_result)/logLik(null_result)

## Odds Ratio
exp(logit_result$coefficients)

## Predicted Probability
logit$predict <- predict(logit_result, logit, type = "response")

## Export Logistic Regression Results 
write.csv(logit, file = file.choose(new=TRUE), row.names = FALSE) ## Name file logit_pred.csv

