## ----setup, include = FALSE----------------------------------------------
source('_format.R')

## ----message=FALSE-------------------------------------------------------
library(FrF2)

FrF2(nfactors=4, resolution=3)

## ------------------------------------------------------------------------
FrF2(nfactors=5, nruns=8)

## ------------------------------------------------------------------------
model <-lm(FiltrationRate ~ A + B + C + D + A:B + A:C + A:D, data=Table8.3)
print(summary(model))

## ------------------------------------------------------------------------
model <-lm(FiltrationRate ~ A + C + D + A:C + A:D, data=Table8.3)
print(summary(model))

## ----message=FALSE-------------------------------------------------------
model <- aov(Yield ~ . * ., data=Table8.5)
model.ss <- anova(model)[['Sum Sq']]
effect.estimates <- data.frame(
	'EffectEstimate'=2 * coef(model)[-1],
	'SumOfSquares'=model.ss[-1],
	'Contribution'=sprintf('%0.2f%%', 100*model.ss[-1]/sum(model.ss[-1]))
)

## ----echo=FALSE----------------------------------------------------------
kable(effect.estimates) %>% kable_styling()

## ----fig.align='center', fig.height=5, fig.width=5, message=FALSE--------
library(DoE.base)
vals <- halfnormal(model)
vals$signif

## ------------------------------------------------------------------------
model <- aov(Yield ~ A + B + C + A:B, data=Table8.5)
print(summary(model))

## ----fig.align='center', fig.width=8, fig.height=4-----------------------
op <- par(mfrow=c(1,2))
qqnorm(resid(model), main='Normal Probability Plot')
qqline(resid(model), col='orange')
plot(x=predict(model), y=resid(model), main='Fitted vs. Residual')
par(op)

## ------------------------------------------------------------------------
model <- lm(Shrinkage ~ . * ., data=Table8.10)
print(summary(model))

## ----fig.align='center', fig.width=5, fig.height=5-----------------------
halfnormal(model)

## ------------------------------------------------------------------------
model <- lm(Shrinkage ~ A + B + A:B, data=Table8.10)
print(summary(model))

## ----fig.align='center', fig.width=8, fig.height=4-----------------------
op <- par(mfrow=c(1,2))
qqnorm(resid(model), main='Normal Probability Plot')
qqline(resid(model), col='orange')
plot(x=Table8.10$C, y=resid(model), main='C vs. Residual')
par(op)

## ----fig.align='center', fig.width=5, fig.height=5-----------------------
ix <- Table8.10$C == 1
X <- model.matrix(Shrinkage ~ -1 + . * ., data=Table8.10)
Fi.star <- apply(
	X,
	2,
	function(w){
		log(
			sd(resid(model)[w == 1])^2 
			/ 
			sd(resid(model)[w != 1])^2
		)
	}
)
vals <- halfnormal(Fi.star)
vals

## ------------------------------------------------------------------------
model <- aov(log(Deviation) ~ Error(Block) + (A + B + C + D + E + F + G + H)^3, data=Table8.16)
print(summary(model))

## ------------------------------------------------------------------------
model.ss <- summary(model$Within)[[1]][['Sum Sq']]
coef.est <- coef(model$Within)
effect.estimates <- data.frame(
	'EffectEstimate'=2 * coef.est,
	'SumOfSquares'=model.ss,
	'Contribution'=sprintf('%0.2f%%', 100*model.ss/sum(model.ss))
)

## ----echo=FALSE----------------------------------------------------------
kable(effect.estimates) %>% kable_styling()

## ----fig.align='center', fig.width=5, fig.height=5-----------------------
model <- aov(log(Deviation) ~ (A + B + C + D + E + F + G + H)^3, data=Table8.16)
vals <- halfnormal(model)
vals$signif

## ------------------------------------------------------------------------
model <- aov(log(Deviation) ~ Error(Block) + A + B + D + A:D, data=Table8.16)
print(summary(model))

## ------------------------------------------------------------------------
model <- lm(Time ~ ., data=Table8.21)
print(2*coef(model)[-1])

## ------------------------------------------------------------------------
df <- rbind(Table8.21, Table8.22)
model <- lm(Time ~ (A + B + D)^2, data=df)
summary(model)

## ------------------------------------------------------------------------
alpha.to.enter <- 0.10
alpha.to.leave <- 0.10
model <- lm(y~1, data=Table8.25)
scope <- y ~ (X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12)^2

final.model <- p.stepwise(model, scope, alpha.to.enter, alpha.to.leave)
summary(final.model)

## ----fig.align='center', fig.width=5, fig.height=5, message=FALSE--------
model <- lm(Thickness ~ . * ., data=Table8.31)
vals <- halfnormal(model)
vals$signif

## ----fig.align='center', fig.width=5, fig.height=5, message=FALSE--------
model <- lm(Thickness ~ Blocks + (A + B + C + D + E + F)^2, data=Table8.32)
vals <- halfnormal(model)
vals$signif

## ----fig.align='center', fig.width=5, fig.height=5, message=FALSE--------
ix <- Table8.32[,'Blocks'] == 1 | (Table8.32[,'Blocks'] == 2 & Table8.32[,'A'] == -1)
Table8.33 <- Table8.32[ix,]
model <- lm(Thickness ~ Blocks + (A + B + C + D + E + F)^2, data=Table8.33)
vals <- halfnormal(model, ME.partial=TRUE)
vals$signif

## ------------------------------------------------------------------------
model <- aov(Thickness ~ Error(Blocks) + (A + B + C + D + E + F)^2, data=Table8.33)
print(summary(model))

