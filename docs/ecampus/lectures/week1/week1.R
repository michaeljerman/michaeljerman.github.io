## Examples from week 1 of ECON 424

## Small sample hypothesis tests
x <- c(15.6, 16.2, 22.5, 20.5, 16.4, 19.4, 16.6, 17.9, 12.7, 13.9)

mean(x)
sd(x)

xbar <- mean(x)
sdx <- sd(x)
n <- length(x)
mu0 <- 20

t <- (xbar - mu0)/(sdx/sqrt(n))

tcrit <- -qt(0.99, n-1)

t
tcrit

## Confidence intervals
x <- c(15.6, 16.2, 22.5, 20.5, 16.4, 19.4, 16.6, 17.9, 12.7, 13.9)

n <- length(x)
tcrit <- qt(0.975, n-1)

xbar <- mean(x)
sdx <- sd(x)

upperci <- tcrit*(sdx/sqrt(n)) + xbar
lowerci <- -tcrit*(sdx/sqrt(n)) + xbar

c(lowerci, upperci)
