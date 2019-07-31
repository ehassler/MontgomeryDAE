## ----setup, include = FALSE----------------------------------------------
source('_format.R')

## ------------------------------------------------------------------------
X <- model.matrix(~ReactionTemperature + CatalystFeedRate, data=Table10.2)
print(X)

## ------------------------------------------------------------------------
y <- Table10.2$Viscosity
print(y)

## ------------------------------------------------------------------------
beta.hat <- solve(t(X) %*% X, t(X) %*% y)
print(beta.hat)

## ------------------------------------------------------------------------
model <- lm(Viscosity ~ ReactionTemperature + CatalystFeedRate, data=Table10.2)
print(summary(model))

## ------------------------------------------------------------------------
Table10.3 <- data.frame(
	'y'=Table10.2$Viscosity,
	'Predicted'=predict(model),
	'Residual'=resid(model),
	'hii'=hatvalues(model),
	'StudentizedResids'=rstudent(model),
	'CooksD'=cooks.distance(model)
)


## ----echo=FALSE----------------------------------------------------------
kable(Table10.3) %>% kable_styling()

## ------------------------------------------------------------------------
model01 <- lm(Viscosity ~ ReactionTemperature, data=Table10.2)
model012 <- lm(Viscosity ~ ReactionTemperature + CatalystFeedRate, data=Table10.2)
print(anova(model01, model012))

## ------------------------------------------------------------------------
model <- lm(Viscosity ~ ReactionTemperature + CatalystFeedRate, data=Table10.2)
confint(model)

## ----fig.align='center', fig.width=5, fig.height=5-----------------------
x1 <- seq(from=80, to=100, length.out=100)
x2 <- rep(10.5, 100)
y <-predict(model, newdata=data.frame('ReactionTemperature'=x1, 'CatalystFeedRate'=x2), interval="confidence") 
plot(x1, y[,1], type='l', ylim=c(2248, 2434), main='Prediction Interval', xlab='Reaction Temperature', ylab='Viscosity')
lines(x1, y[,2], lty=2)
lines(x1, y[,3], lty=2)

