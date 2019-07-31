## ----setup, include = FALSE----------------------------------------------
source('_format.R')

## ------------------------------------------------------------------------
options(contrasts=c('unordered'='contr.sum', 'ordered'='contr.poly'))

## ------------------------------------------------------------------------
str(Table3.1)

## ------------------------------------------------------------------------
summary(aov(Observation ~ Power, data=Table3.1))

## ----message=FALSE-------------------------------------------------------
model <- lm(Observation ~ Power, data=Table3.1)
summary(model)

## ----fig.align='center', fig.width=5, fig.height=5-----------------------
qqnorm(resid(model))
qqline(resid(model), col='orange')

## ----fig.align='center', fig.height=4.5, fig.width=8---------------------
op <- par(mfrow=c(1, 2))
plot(resid(model), main='Residual vs. Run Order')
plot(x=predict(model), y=resid(model), main='Predicted vs. Residual')
par(op)

## ------------------------------------------------------------------------
bartlett.test(Observation ~ Power, data=Table3.1)

## ------------------------------------------------------------------------
str(Example3.5)

## ----message=FALSE-------------------------------------------------------
model <- aov(Observation ~ EstimationMethod, data=Example3.5)
summary(model)

## ----fig.align='center', fig.width=4, fig.height=4-----------------------
plot(predict(model), resid(model), main='Predicted vs. Residual')

## ------------------------------------------------------------------------
y <- sqrt(Example3.5$Observation)
model <- aov(y ~ EstimationMethod, data=Example3.5)
summary(model)

## ----fig.align='center', fig.width=4, fig.height=4-----------------------
plot(predict(model), resid(model), main='Predicted vs. Residual')

## ----message=FALSE-------------------------------------------------------
x <- as.numeric(as.character(Table3.1$Power))
linear.model <- lm(Observation ~ x, data=Table3.1)
quadratic.model <- lm(Observation ~ x + I(x^2), data=Table3.1)

## ----message=FALSE, fig.align='center', fig.width=8, fig.height=4--------
op <- par(mfrow=c(1,2))
plot(x, Table3.1$Observation, main='Linear Model')
abline(a=coef(linear.model)[1], b=coef(linear.model)[2], col='orange')
plot(x, Table3.1$Observation, main='Quadratic Model')
u <- seq(from=160, to=220, length.out=100)
v <- cbind(rep(1, length(u)), u, u^2) %*% coef(quadratic.model)
lines(u, v, col='orange') 
par(op)

## ----message=FALSE-------------------------------------------------------
model <- aov(Observation ~ Power, data=Table3.1)
summary(model)

## ----message=FALSE-------------------------------------------------------
library(emmeans)
emmodel <- emmeans(model,	~ Power)
contrast(emmodel, list(
	'C1'=c(1, -1, 0, 0),
	'C2'=c(1, 1, -1, -1),
	'C3'=c(0, 0, 1, -1)
))

## ------------------------------------------------------------------------
pairs(emmodel, adjust="tukey")

## ------------------------------------------------------------------------
library(pwr)
w <- c(575, 600, 650, 675)
a <- length(w)
sigma <- 25
f <- sqrt((1/a)* sum((w - mean(w))^2)) / sigma
pwr.anova.test(k=4, f=f, n=3, sig.level=0.01)

## ----fig.align='center', fig.width=5, fig.height=5-----------------------
library(pwr)
n <- seq(from=2, to=8, length.out=50)
y <- pwr.anova.test(k=4, f=f, sig.level=0.01, n=n)$power
plot(n, y, type='l', 
	 main='Power for Example 3.1', 
	 xlab='Sample Size', 
	 ylab='Power')

## ------------------------------------------------------------------------
pwr.anova.test(k=4, f=f, sig.level=0.01, power=0.9)

## ----fig.align='center', fig.width=5, fig.height=5-----------------------
boxplot(Observation ~ Factor, data=Table3.12)

## ----message=FALSE-------------------------------------------------------
summary(aov(Observation ~ Factor, data=Table3.12))

## ------------------------------------------------------------------------
means.model <- lm(Observation ~ Factor - 1, data=Table3.12)
print(summary(means.model))

## ------------------------------------------------------------------------
confint(means.model)

## ------------------------------------------------------------------------
pairwise.model <- pairs(emmeans(aov(Observation ~ Factor, data=Table3.12), ~Factor), adjust='none')
print(pairwise.model)

## ------------------------------------------------------------------------
confint(pairwise.model)

## ----fig.align='center', fig.width=8, fig.height=4-----------------------
op <- par(mfrow=c(1,2))
qqnorm(resid(means.model), main='Normal Probability Plot')
qqline(resid(means.model), col='orange')
plot(x=predict(means.model), y=resid(means.model), main='Fitted vs. Residual')
par(op)

## ------------------------------------------------------------------------
str(Table3.14)

## ----fig.align='center', fig.width=5, fig.height=5-----------------------
boxplot(Observations ~ DisplayDesign, data=Table3.14)

## ----message=FALSE-------------------------------------------------------
model <- aov(Observations ~ DisplayDesign, data=Table3.14)
summary(model)

## ------------------------------------------------------------------------
means.model <- lm(Observations ~ DisplayDesign - 1, data=Table3.14)
summary(means.model)

## ------------------------------------------------------------------------
confint(means.model)

## ------------------------------------------------------------------------
ls.model <- emmeans(model, ~DisplayDesign)
print(ls.model)

## ------------------------------------------------------------------------
confint(ls.model)

## ------------------------------------------------------------------------
pairwise.model <- pairs(ls.model, adjust='none')
print(pairwise.model)

## ------------------------------------------------------------------------
confint(pairwise.model)

## ----fig.align='center', fig.width=8, fig.height=4-----------------------
op <- par(mfrow=c(1,2))
qqnorm(resid(means.model), main='Normal Probability Plot')
qqline(resid(means.model), col='orange')
plot(x=predict(means.model), y=resid(means.model), main='Fitted vs. Residual')
par(op)

## ------------------------------------------------------------------------
str(Table3.16)

## ----message=FALSE-------------------------------------------------------
y <- -log(Table3.16$StDevVoltage)
summary(aov(y ~ RatioControlAlgorithm, data=Table3.16))

## ----fig.align='center', fig.width=5, fig.height=5-----------------------
boxplot(y ~ RatioControlAlgorithm, data=Table3.16)

## ------------------------------------------------------------------------
model <- aov(Strength ~ Error(Looms), data=Example3.10)
summary(model)

## ----message=FALSE-------------------------------------------------------
library(lme4)
library(lmerTest)

model <- lmer(
	Strength ~ (1|Looms),
	data=Example3.10,
	REML=TRUE
)
print(summary(model))
print(rand(model))

## ----message=FALSE-------------------------------------------------------
confint(model, oldNames=FALSE)

## ----message=FALSE-------------------------------------------------------
kruskal.test(Observation ~ Power, data=Table3.1)

