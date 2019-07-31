## ----setup, include = FALSE----------------------------------------------
source('_format.R')

## ----message=FALSE-------------------------------------------------------
library(lme4)
library(lmerTest)

model <- lmer(Purity ~ (1 | Supplier/Batches), data=Table14.3)
print(summary(model))
print(rand(model))

## ------------------------------------------------------------------------
df <- Table14.10
df[,'FO'] <- factor(sprintf('F%s.O%s', as.character(df[,'Fixture']), as.character(df[,'Operator'])))
model <- lmer(AssemblyTime ~ Layout * Fixture + (1|Operator/Layout) + (1|FO/Layout), data=df)
print(anova(model))

## ------------------------------------------------------------------------
print(summary(model))

## ------------------------------------------------------------------------
print(rand(model))

## ------------------------------------------------------------------------
df <- Table14.16
df[,'Temperature'] <- factor(df[,'Temperature'])
model <- aov(TensileStrength ~ Temperature * Preperation + Error(Replicate), data=df)
summary(model)

## ------------------------------------------------------------------------
library(lme4)
library(lmerTest)

df <- Table14.16
df[,'Temperature'] <- factor(df[,'Temperature'])
model <- lmer(TensileStrength ~ Temperature * Preperation * (1|Replicate), data=df)
print(anova(model))
print(summary(model))
print(rand(model))

## ------------------------------------------------------------------------
model <- aov(Uniformity ~ (A + B + C + D + E)^2 +  Error(WholePlot), data=Table14.22)
coef(model)

## ----fig.align='center', fig.width=8, fig.height=4-----------------------
op <- par(mfrow=c(1,2))
# Whole plot
labs <- names(coef(model)$WholePlot)
pts <-qqnorm(coef(model)$WholePlot, col='white', main='WholePlot')
qqline(pts$y, col='orange')
text(x=pts$x, y=pts$y, labels=labs, cex=0.5)
# Sub plot
labs <- names(coef(model)$Within)
pts <-qqnorm(coef(model)$Within, col='white', main='Subplot')
qqline(pts$y, col='orange')
text(x=pts$x, y=pts$y, labels=labs, cex=0.5)
par(op)

