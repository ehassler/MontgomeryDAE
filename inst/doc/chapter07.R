## ----setup, include = FALSE----------------------------------------------
source('_format.R')

## ------------------------------------------------------------------------
df <- Figure6.1
df[,'Block'] <- factor(c(rep(1, 4), rep(2, 4), rep(3, 4)))

model <- aov(Yield ~ Error(Block) + A * B, data=df)
summary(model)

## ----message=FALSE-------------------------------------------------------
library(lme4)
library(lmerTest)
model <- lmer(
    Yield ~ A * B + (1|Block),
    data=df,
    REML=TRUE
)
summary(model)

## ------------------------------------------------------------------------
rand(model)

## ----message=FALSE-------------------------------------------------------
confint(model, oldNames=FALSE)

## ------------------------------------------------------------------------
df <- Table6.10
df[df$Block == 1, 'Filtration'] <- df[df$Block == 1, 'Filtration'] - 20

model <- aov(Filtration ~ Temperature * Pressure * Formaldehyde * StirringRate, data=Table6.10)
model.ss <- anova(model)[['Sum Sq']]
effect.estimates <- data.frame(
	'EffectEstimate'=2 * coef(model)[-1],
	'SumOfSquares'=model.ss[-1],
	'Contribution'=sprintf('%0.2f%%', 100*model.ss[-1]/sum(model.ss[-1]))
)

## ----echo=FALSE----------------------------------------------------------
kable(effect.estimates) %>% kable_styling(font_size=10)

## ------------------------------------------------------------------------
model <- aov(Filtration ~ Error(Block) + Temperature + Formaldehyde + StirringRate + Temperature:Formaldehyde + Temperature:StirringRate, data=df)
summary(model)

## ------------------------------------------------------------------------
model <- aov(EtchRate ~ Error(Block) + Gap * Flow * Power, data=Table6.4)
summary(model)

