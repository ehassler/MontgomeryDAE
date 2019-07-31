## ----setup, include = FALSE----------------------------------------------
source('_format.R')

## ----fig.align='center', fig.width=5, fig.height=5-----------------------
library(MASS)

model <- lm(Observation ~ EstimationMethod, data=Example3.5)
vals <- boxcox(model)

## ------------------------------------------------------------------------
best.lambda <- vals$x[which.max(vals$y)]
print(best.lambda)

## ------------------------------------------------------------------------
model <- glm(cbind(Coupons, I(1000 - Coupons)) ~ (A + B + C)^2, data=Table15.1, family='binomial')
print(summary(model))

## ------------------------------------------------------------------------
df <- Problem8.51[,c('A','B','C','D','E','F','G','H','J')]
df[,'Defects'] <- round(Problem8.51[,'Sqrt']^2)

## ------------------------------------------------------------------------
model <- glm(Defects ~ D + F + B:G, data=df, family='poisson')
summary(model)

## ------------------------------------------------------------------------
response <- predict(model, type='response', se.fit=TRUE)
2 * qnorm(0.975) * response$se.fit

## ------------------------------------------------------------------------
model <- glm(CyclesToFailure ~ x1 + x2 + x3, data=Table15.4, family=Gamma(link='log'))
summary(model)

## ------------------------------------------------------------------------
response <- predict(model, type='response', se.fit=TRUE)
2 * qnorm(0.975) * response$se.fit

## ------------------------------------------------------------------------
model <- aov(y ~ I(x - mean(x)) + Machine, data=Table15.10)
print(summary(model))

## ------------------------------------------------------------------------
op <- options(contrast=c('contr.sum', 'contr.poly'))
model <- lm(y ~ I(x - mean(x)) * (A + B + C)^2, data=Table15.16)
options(op)
print(summary(model))

## ------------------------------------------------------------------------
xx <- (Table15.16$x - mean(Table15.16$x))
op <- options(contrast=c('contr.sum', 'contr.poly'))
model <- aov(y ~ xx + A + B + C + A:B + A:xx + B:xx + A:B:xx, data=Table15.16)
options(op)
print(summary(model))

