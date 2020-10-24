library(wooldridge)
data(wage2)

## Estimate simple regression model
slr <- lm(wage ~ educ, data=wage2)

## Include experience as explanatory variable
mlr <- lm(wage ~ educ + exper, data=wage2)

## Create a random variable of length n, add it to wage2 dataframe
n <- length(wage2$wage)

wage2$random_variable <- rnorm(n)

## Estimate the mlr model with the random variable
mlr_random <- lm(wage ~ educ + random_variable, data=wage2)
summary(mlr_random)

## Estimate models to compare coefficients
tilde_reg <- lm(wage ~ educ, data=wage2)
hat_reg <- lm(wage ~ educ + exper, data=wage2)
delta_reg <- lm(exper ~ educ, data=wage2)

## Extract coefficients from models
beta1tilde <- coefficients(tilde_reg)[2]
beta1hat <- coefficients(hat_reg)[2]
beta2hat <- coefficients(hat_reg)[3]
delta1hat <- coefficients(delta_reg)[2]

## Compare coefficients
c(beta1tilde, beta1hat + beta2hat*delta1hat)

## Create linear dependent variable
wage2$post_highschool <- wage2$educ - 12
summary(wage2$post_highschool)

## Estimate the colinear model
colinear_reg <- lm(wage ~ educ + post_highschool, wage2)
summary(colinear_reg)

## Calculate OVB
beta2hat*delta1hat
