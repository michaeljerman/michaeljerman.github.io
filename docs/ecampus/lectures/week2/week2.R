library(wooldridge)
library(ggplot2)

## Load the wage2 dataset
data(wage2)

## Create a scatterplot of wage vs education
qplot(x=educ, y=wage, data=wage2)

## Calculate beta 1 hat:
beta1_hat <- cov(wage2$educ, wage2$wage)/var(wage2$educ)

## Calculate beta 0 hat
wage_bar <- mean(wage2$wage)

educ_bar <- mean(wage2$educ)

wage_bar - beta1_hat*educ_bar

## OLS:
wage_reg <- lm(wage ~ educ, data=wage2)
summary(wage_reg)

## Plot residuals
qplot(x=wage2$educ, y=wage_reg$residuals)

## Create logged variables
wage2$leduc <- log(wage2$educ)
