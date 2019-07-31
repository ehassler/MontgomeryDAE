## ----setup, include = FALSE----------------------------------------------
source('_format.R')

## ------------------------------------------------------------------------
str(Table4.3)

## ----message=FALSE-------------------------------------------------------
model <- aov(Flicks ~ Error(BatchOfResin) + ExtrusionPressure, data=Table4.3)
print(summary(model))

## ------------------------------------------------------------------------
summary(aov(Flicks ~ ExtrusionPressure, data=Table4.3))

## ------------------------------------------------------------------------
library(emmeans)
model <- aov(Flicks ~ ExtrusionPressure + BatchOfResin, data=Table4.3)
exmodel <- emmeans(model, ~ ExtrusionPressure)
print(exmodel)
blmodel <- emmeans(model, ~ BatchOfResin)
print(blmodel)

## ----fig.align='center', fig.width=5, fig.height=5-----------------------
boxplot(Flicks ~ ExtrusionPressure, data=Table4.3)

## ----fig.align='center', fig.width=8, fig.height=4-----------------------
op <- par(mfrow=c(1,2))
qqnorm(resid(model), main='Normal Probability Plot')
qqline(resid(model), col='orange')
plot(x=predict(model), y=resid(model), main='Fitted vs. Residual')
par(op)

## ----fig.align='center', fig.width=8, fig.height=4-----------------------
op <- par(mfrow=c(1,2))
plot(y=resid(model), x=as.numeric(Table4.3$ExtrusionPressure), main='Residual vs. Extrusion Pressure')
plot(y=resid(model), x=as.numeric(Table4.3$BatchOfResin), main='Residual vs. Catch')
par(op)

## ----message=FALSE-------------------------------------------------------
library(lme4)
library(lmerTest)
model <- lmer(
	Flicks ~ -1 + ExtrusionPressure + (1|BatchOfResin),
	data=Table4.3,
	REML=TRUE
)
print(summary(model))
print(rand(model))

## ----message=FALSE-------------------------------------------------------
confint(model, oldNames=FALSE)

## ------------------------------------------------------------------------
str(Table4.9)

## ------------------------------------------------------------------------
y <- Table4.9$BurnRate - 25
model <- aov(y ~ Error(Batch + Operator) + Formulation, data=Table4.9)
summary(model)

## ------------------------------------------------------------------------
library(agricolae)

trt <- c("A", "B", "C", "D")
outdesign <- design.lsd(trt)
print(outdesign$sketch)

## ------------------------------------------------------------------------
y <- Table4.20$BurnRate - 25
model <- aov(y ~ Error(Batch + Operator + TestAssembly) + Formulation, data=Table4.20)
summary(model)

## ----message=FALSE-------------------------------------------------------
model <- aov(ReactionTime ~ Batch + Catalyst, data=Table4.22)
summary(model)

## ----message=FALSE-------------------------------------------------------
model <- aov(ReactionTime ~ Catalyst + Batch, data=Table4.22)
summary(model)

## ------------------------------------------------------------------------
library(emmeans)
model <- aov(ReactionTime ~ Batch + Catalyst, Table4.22)
print(summary(model))
camodel <- emmeans(model, ~Catalyst)
print(camodel)
bamodel <- emmeans(model, ~Batch)
print(bamodel)
print(pairs(camodel, adjust='tukey'))

